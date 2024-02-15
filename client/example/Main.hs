{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (Message (..))
import Katip
import Katip.Monadic
import KatipLogger (katipLogger)
import Network.GRPC.Client (CIHeaderList)
import Network.HTTP2.Frame (ErrorCode)
import Safe (readMay)
import SimpleLogger (simpleLogger)
import System.Environment (getArgs)
import UnliftIO (MonadIO, bracket, stdout, throwString)
import Utxorpc.Client
  ( BuildClient (..),
    ServerStreamReply,
    SubmitClient (..),
    SyncClient (..),
    UnaryReply,
    UtxorpcClient (..),
    UtxorpcClientLogger,
    UtxorpcInfo (..),
    utxorpcClient,
  )
import "http2-client" Network.HTTP2.Client (ClientError, TooMuchConcurrency)

-- Get server info from args pass to `runUtxo`
main :: IO ()
main =
  do
    args <- getArgs
    case parseInfo args of
      Left err -> putStrLn err >> putStrLn usageStr
      Right serviceInfo ->
        if "--katip" `elem` args
          then runKatipExample serviceInfo
          else runSimpleExample serviceInfo
  where
    -- Parse command line args for server info
    parseInfo :: [String] -> Either String (UtxorpcInfo m)
    parseInfo args =
      case filter (not . (==) '-' . head) args of
        (hostName : portStr : tlsStr : gzipStr : hdrs) ->
          UtxorpcInfo hostName
            <$> parse ("Invalid port number: " ++ portStr) portStr
            <*> parse ("Invalid tlsEnabled arg: " ++ tlsStr) tlsStr
            <*> parse ("Invalid useGzip arg: " ++ gzipStr) gzipStr
            <*> parsedHeaders hdrs
            <*> pure Nothing
        invalidArgs -> Left $ "Not enough args (" ++ show (length invalidArgs) ++ " received)."
      where
        parse msg str = case readMay str of
          Nothing -> Left msg
          Just val -> Right val

    parsedHeaders :: [String] -> Either String [(BS.ByteString, BS.ByteString)]
    parsedHeaders = mapM (mkPair . BS.split ':' . BS.pack)
      where
        mkPair :: [BS.ByteString] -> Either String (BS.ByteString, BS.ByteString)
        mkPair [k, v] = Right (k, v)
        mkPair headerStr =
          Left $
            "Invalid header key:value pair: " ++ show (BS.unpack <$> headerStr)

    usageStr = "Usage: [--katip] <hostName> <port> <tlsEnabled> <useGzip> [<headerKey>:<headerValue> [...]]"

runKatipExample :: UtxorpcInfo m -> IO ()
runKatipExample serviceInfo = do
  bracket mkLogEnv closeScribes $ \le -> do
    eService <- utxorpcClient $ serviceInfo {_logger = Just $ mkKatipLogger le}
    case eService of
      Left clientErr -> handleClientErr clientErr
      Right service -> runUtxo service
  where
    -- Make KatipLogger. LogEnv required for the `unlift` function
    mkKatipLogger :: LogEnv -> UtxorpcClientLogger (KatipContextT IO)
    mkKatipLogger le = katipLogger $ KatipContextTState le mempty "example"

    -- Make log environment for KatipLogger
    mkLogEnv :: IO LogEnv
    mkLogEnv = do
      handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      le <- initLogEnv "Utxorpc" "development"
      registerScribe "stdout" handleScribe defaultScribeSettings le

runSimpleExample :: UtxorpcInfo m -> IO ()
runSimpleExample serviceInfo = do
  eService <- utxorpcClient $ serviceInfo {_logger = Just simpleLogger}
  case eService of
    Left clientErr -> handleClientErr clientErr
    Right service -> runUtxo service

-- Make UTxO RPC calls with empty messages
-- Errors are handled by throwing IO exceptions and exiting
-- `handleStream` is the stream handler function expected by a `ServerStreamCall`.
-- Note that the type of `handleStream` has nothing to do with the type of the
-- logger.
runUtxo :: UtxorpcClient -> IO ()
runUtxo client = do
  _maybeFetchBlockResponse <- handleUnaryReply $ fetchBlock (syncClient client) defMessage
  _maybeChainTipResponse <- handleUnaryReply $ getChainTip (buildClient client) defMessage
  _maybeStreamState <-
    handleStreamReply $
      watchMempool (submitClient client) (0 :: Int) defMessage handleStream
  return ()
  where
    handleStream n _headerList _reply = do
      putStrLn ("The stream handler is processing message #" ++ show n)
      return (n + 1)

-- Composite handlers for each error type that can be returned by the

handleUnaryReply :: UnaryReply o -> IO (Maybe o)
handleUnaryReply reply = do
  res <- reply
  case res of
    Left clientErr ->
      handleClientErr clientErr
    Right (Left tmc) ->
      handleTMC tmc
    Right (Right (Left errCode)) ->
      handleHTTP2Err errCode
    Right (Right (Right (headers, trailers, Left errMsg))) ->
      handleServerError (headers, trailers, errMsg)
    Right (Right (Right (_, _, Right msg))) ->
      return $ Just msg

handleStreamReply :: ServerStreamReply a -> IO (Maybe a)
handleStreamReply runReply = do
  reply <- runReply
  case reply of
    Left clientErr ->
      handleClientErr clientErr
    Right (Left tmc) ->
      handleTMC tmc
    Right (Right (streamState, _headerList, _trailerList)) -> return $ Just streamState

-- Error handlers that throw IO exceptions
-- A production application should handle these more gracefully
-- E.g., TooMuchConcurrency errors indicate a need to wait and
-- try again.

handleClientErr :: (MonadIO m) => ClientError -> m a
handleClientErr _ = throwString "Early end of stream!"

handleTMC :: (MonadIO m) => TooMuchConcurrency -> m a
handleTMC _ = throwString "Too much concurrency!"

handleHTTP2Err :: (MonadIO m) => ErrorCode -> m a
handleHTTP2Err errCode = throwString $ "HTTP2 Error: " ++ show errCode

handleServerError :: (MonadIO m) => (CIHeaderList, Maybe CIHeaderList, String) -> m a
handleServerError (_headers, _trailers, msg) = throwString $ "Received error message: " ++ msg
