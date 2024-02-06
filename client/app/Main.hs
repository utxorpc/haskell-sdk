{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (Message (..))
import qualified Data.String as BS
import Katip
import Katip.Monadic
import KatipLogger (katipLogger)
import Network.GRPC.Client (CIHeaderList)
import Network.HTTP2.Frame (ErrorCode)
import Safe (readMay)
import SimpleLogger (simpleLogger)
import System.Environment (getArgs)
import UnliftIO (MonadIO, bracket, stdout, throwString)
import Utxorpc.Client (ServiceInfo (..), utxorpcService)
import Utxorpc.Logged (UtxorpcClientLogger)
import Utxorpc.Types
  ( BuildServiceImpl (getChainTip),
    ServerStreamReply,
    SubmitServiceImpl (watchMempool),
    SyncServiceImpl (fetchBlock),
    UnaryReply,
    UtxorpcService (buildS, submitS, syncS),
  )
import "http2-client" Network.HTTP2.Client (ClientError, TooMuchConcurrency)

-- Get server info from args pass to `runUtxo`
main :: IO ()
main =
  do
    eInfo <- parseInfo <$> getArgs
    case eInfo of
      Left err -> putStrLn err >> putStrLn usageStr
      Right serviceInfo -> do
        bracket mkLogEnv closeScribes $ \le -> do
          eService <- utxorpcService $ serviceInfo {logger = Just $ mkKatipLogger le}
          either
            (const $ putStrLn "Early end of stream before client was established.")
            runUtxo
            eService
  where
    -- Make KatipLogger. LogEnv required for the `unlift` function
    mkKatipLogger :: LogEnv -> UtxorpcClientLogger (KatipContextT IO)
    mkKatipLogger le = katipLogger $ KatipContextTState le mempty "example"

    -- Make log environment for KatipLogger
    mkLogEnv = do
      handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
      le <- initLogEnv "Utxorpc" "development"
      registerScribe "stdout" handleScribe defaultScribeSettings le

    -- Parse command line args for server info
    parseInfo (hostName : portStr : tlsStr : gzipStr : hdrs) =
      ServiceInfo hostName
        <$> parse ("Invalid port number: " ++ portStr) portStr
        <*> parse ("Invalid tlsEnabled arg: " ++ tlsStr) tlsStr
        <*> parse ("Invalid useGzip arg: " ++ gzipStr) gzipStr
        <*> parsedHeaders hdrs
        <*> pure (Just simpleLogger)
      where
        parse msg str = case readMay str of
          Nothing -> Left msg
          Just val -> Right val
    parseInfo _ = Left "Not enough args."

    parsedHeaders :: [String] -> Either String [(BS.ByteString, BS.ByteString)]
    parsedHeaders = mapM (mkPair . BS.split ':' . BS.fromString)
      where
        mkPair :: [BS.ByteString] -> Either String (BS.ByteString, BS.ByteString)
        mkPair [k, v] = Right (k, v)
        mkPair headerStr =
          Left $
            "Invalid header key:value pair: " ++ show (BS.unpack <$> headerStr)

    usageStr = "Usage: <hostName> <port> <tlsEnabled> <useGzip> [<headerKey>:<headerValue> [...]]"

-- Make UTxO RPC calls with empty messages
-- Errors are handled by throwing IO exceptions and exiting
-- `handleStream` is the stream handler function expected by a `ServerStreamCall`.
-- Note that the type of `handleStream` has nothing to do with the type of the
-- logger.
runUtxo :: UtxorpcService -> IO ()
runUtxo service = do
  _maybeFetchBlockResponse <- handleUnaryReply $ fetchBlock (syncS service) defMessage
  _maybeChainTipResponse <- handleUnaryReply $ getChainTip (buildS service) defMessage
  _maybeStreamState <-
    handleStreamReply $
      watchMempool (submitS service) (0 :: Int) defMessage handleStream
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
    Right (Right (Right (headers, trailers, Left msg))) ->
      handleServerError (headers, trailers, msg)
    Right (Right (Right (_, _, Right fetchBlockResponse))) ->
      return $ Just fetchBlockResponse

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
