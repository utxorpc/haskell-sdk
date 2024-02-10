{-# LANGUAGE RankNTypes #-}

module SimpleLogger (simpleLogger) where

import Control.Lens (over, _1)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI (..))
import Data.Time (getZonedTime)
import Data.UUID (UUID)
import Network.Wai (Request (..))
import Utxorpc.Server (ServerStreamEndLogger, ServerStreamLogger, UtxorpcServerLogger (..))

simpleLogger :: (MonadIO m) => UtxorpcServerLogger m
simpleLogger =
  UtxorpcServerLogger
    simpleRequestLogger
    simpleReplyLogger
    simpleServerStreamLogger
    simpleServerStreamEndLogger

simpleRequestLogger :: (Show i, MonadIO m) => BS.ByteString -> Request -> UUID -> i -> m ()
simpleRequestLogger rpcPath req uuid i =
  liftIO $
    logEvent "REQUEST" rpcPath req uuid Nothing i

simpleReplyLogger :: (Show o, MonadIO m) => BS.ByteString -> Request -> UUID -> o -> m ()
simpleReplyLogger rpcPath req uuid o =
  liftIO $
    logEvent "REQUEST" rpcPath req uuid Nothing o

simpleServerStreamLogger :: (MonadIO m) => ServerStreamLogger m
simpleServerStreamLogger rpcPath req (uuid, index) o =
  liftIO $
    logEvent "SERVER STREAM" rpcPath req uuid (Just index) o

simpleServerStreamEndLogger :: (MonadIO m) => ServerStreamEndLogger m
simpleServerStreamEndLogger rpcPath req (uuid, index) =
  liftIO $
    logEvent "SERVER STREAM END" rpcPath req uuid (Just index) "End of server stream"

logEvent ::
  (Show a) =>
  String ->
  BS.ByteString ->
  Request ->
  UUID ->
  Maybe Int ->
  a ->
  IO ()
logEvent eventName rpcPath req uuid index msg = do
  (header, footer) <- headerFooter eventName
  putStrLn header
  putStrLn $ "UUID: " ++ show uuid
  case index of
    Just i -> putStrLn $ "Message #" ++ show i
    Nothing -> return ()
  putStrLn $ "Path: " ++ BS.unpack rpcPath
  putStrLn $ "Remote: " ++ show (remoteHost req)
  unless (null $ requestHeaders req) $
    liftIO . putStr $
      showCIHdrs "Headers" (requestHeaders req)
  liftIO . putStrLn $ "Message\n" ++ indent 1 (show msg)
  putStrLn footer

headerFooter :: String -> IO (String, String)
headerFooter eventName = do
  zoned <- getZonedTime
  return (header zoned, footer)
  where
    header zoned = replicate lPadding c ++ " " ++ hdrText ++ " " ++ replicate rPadding c
      where
        hdrText = eventName ++ ": " ++ show zoned
        lPadding = diff `div` 2
        rPadding = diff `div` 2 + (diff `mod` 2)
        diff = w - 2 - length hdrText

    footer = replicate w c

    w = 75
    c = '-'

showHdrs :: String -> [(BS.ByteString, BS.ByteString)] -> String
showHdrs name hdrs =
  name
    ++ ":\n"
    ++ (if not (null hdrs) then indent 1 . unlines $ map showHdr hdrs else "")
  where
    showHdr (k, v) = BS.unpack k ++ ": " ++ BS.unpack v

showCIHdrs :: String -> [(CI BS.ByteString, BS.ByteString)] -> String
showCIHdrs name hdrs = showHdrs name $ map (over _1 original) hdrs

indent :: Int -> String -> String
indent n = unlines . map (replicate n '\t' ++) . lines
