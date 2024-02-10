{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified BuildImpl
import Data.List (find, isPrefixOf)
import Katip (Severity (..), closeScribes, logTM, ls)
import KatipLogger (katipLogger, mkLogEnv, unliftKatip)
import Network.GRPC.HTTP2.Encoding (Compression, gzip)
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS (TLSSettings, defaultTlsSettings)
import SimpleLogger (simpleLogger)
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
  if "--katip" `elem` args
    then runKatipExample defaultTlsSettings (setPort port defaultSettings) [gzip]
    else runSimpleExample

runKatipExample :: TLSSettings -> Settings -> [Compression] -> IO ()
runKatipExample tlsSettings warpSettings compression =
  bracket mkLogEnv closeScribes $ \le -> do
    runUtxorpc $ config le
  where
    config le =
      ServiceConfig
        tlsSettings
        warpSettings
        (handlersImpl katipLogF)
        (Just katipLogger)
        (unliftKatip le)
        compression

    katipLogF str = $(logTM) InfoS (ls str)

runSimpleExample :: IO ()
runSimpleExample = do
  runUtxorpc $
    ServiceConfig
      defaultTlsSettings
      defaultSettings
      (handlersImpl putStrLn)
      (Just simpleLogger)
      id
      [gzip]

handlersImpl :: (MonadIO m) => (String -> m ()) -> UtxorpcHandlers m Int Int Int Int Int
handlersImpl logF =
  UtxorpcHandlers
    (BuildImpl.handlerImpls logF)
    (SubmitImpl.handlerImpls logF)
    (SyncImpl.handlerImpls logF)
    (WatchImpl.handlerImpls logF)
