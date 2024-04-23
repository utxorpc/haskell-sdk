{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Lens.Operators ((&), (.~))
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (Message (..))
import qualified Data.Text.Encoding as TE
import Katip
import Katip.Monadic
import KatipLogger (katipLogger)
import Network.GRPC.Client (CIHeaderList)
import Network.HTTP2.Frame (ErrorCode)
import Proto.Utxorpc.V1alpha.Sync.Sync_Fields
import Safe (readMay)
import SimpleLogger (simpleLogger)
import System.Environment (getArgs)
import UnliftIO (MonadIO, bracket, stdout, throwString)
import Utxorpc.Client
  ( QueryClient (..),
    ServerStreamReply,
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
    let mkEService = utxorpcClient $ serviceInfo {_logger = Just $ mkKatipLogger le}
    bracket mkEService closeService $ \case
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
  let mkEService = utxorpcClient $ serviceInfo {_logger = Just simpleLogger}
  bracket mkEService closeService $ \case
    Left clientErr -> handleClientErr clientErr
    Right service -> runUtxo service

closeService :: Either ClientError UtxorpcClient -> IO ()
closeService (Left _) = return ()
closeService (Right service) = do
  putStrLn "Closing connection"
  void $ close service

-- Make UTxO RPC calls with empty messages
-- Errors are handled by throwing IO exceptions and exiting
-- `handleStream` is the stream handler function expected by a `ServerStreamCall`.
-- Note that the type of `handleStream` has nothing to do with the type of the
-- logger.
runUtxo :: UtxorpcClient -> IO ()
runUtxo client = do
  _maybeChainTipResponse <- handleUnaryReply $ dumpHistory (syncClient client) dumpHistoryRequest
  _maybeReadUtxosResponse <- handleUnaryReply $ readUtxos (queryClient client) defMessage
  _maybeStreamState <-
    handleStreamReply $
      followTip (syncClient client) (0 :: Int) followTipRequest handleStream
  return ()
  where
    dumpHistoryRequest =
      defMessage
        & startToken .~ blockRef
        & maxItems .~ 3

    followTipRequest =
      defMessage
        & intersect
          .~ [ blockRef,
               defMessage
                 & index .~ 41562539
                 & hash
                   .~ TE.encodeUtf8
                     "e4599d275375e54257e7fd922d2f486cf47dd90692d1a7e531804a4e90893346"
             ]

    blockRef =
      defMessage
        & index .~ 41561535
        & hash
          .~ TE.encodeUtf8
            "91ec40dfc09449d918ec7b5311d5ddba318e8a7c337eaf23c9916c6463c30fbe"

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
