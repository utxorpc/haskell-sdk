{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import qualified BuildImpl
import Data.List (find, isPrefixOf)
import Katip (closeScribes)
import KatipLogger (katipLogger, mkLogEnv, unliftKatip)
import Network.GRPC.HTTP2.Encoding (Compression, gzip)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (TLSSettings, defaultTlsSettings)
import qualified SubmitImpl
import qualified SyncImpl
import System.Environment (getArgs)
import UnliftIO (MonadIO, bracket)
import Utxorpc.Server (ServiceConfig (..), UtxorpcHandlers (UtxorpcHandlers), runUtxorpc)
import qualified WatchImpl

main :: IO ()
main = do
  args <- getArgs
  let port = maybe 3000 (read . drop 2) $ find ("-p" `isPrefixOf`) args
  putStrLn $ "Starting server on port " ++ show port
  runKatipExample defaultTlsSettings (setPort port defaultSettings) [gzip]

handlersImpl :: (MonadIO m) => UtxorpcHandlers m Int Int Int Int Int
handlersImpl =
  UtxorpcHandlers
    BuildImpl.handlerImpls
    SubmitImpl.handlerImpls
    SyncImpl.handlerImpls
    WatchImpl.handlerImpls

runKatipExample :: TLSSettings -> Settings -> [Compression] -> IO ()
runKatipExample tlsSettings warpSettings compression =
  bracket mkLogEnv closeScribes $ \le -> do
    runUtxorpc $ config le
  where
    config le =
      ServiceConfig
        tlsSettings
        warpSettings
        handlersImpl
        (Just katipLogger)
        (unliftKatip le)
        compression
