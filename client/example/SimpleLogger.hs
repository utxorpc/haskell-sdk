module SimpleLogger (simpleLogger) where

import Control.Lens.Combinators (over, _1)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI (..))
import Data.Time.LocalTime (getZonedTime)
import Data.UUID (UUID)
import Network.GRPC.Client.Helpers (GrpcClient (..))
import Network.GRPC.HTTP2.Encoding (Compression (..))
import Utxorpc.Client (ReplyLogger, RequestLogger, ServerStreamEndLogger, ServerStreamLogger, UtxorpcClientLogger (..))

-- Prints simple, human-readable messages
simpleLogger :: UtxorpcClientLogger IO
simpleLogger =
  UtxorpcClientLogger
    simpleRequestLogger
    simpleReplyLogger
    simpleServerStreamLogger
    simpleServerStreamEndLogger
    id

simpleRequestLogger :: RequestLogger IO
simpleRequestLogger rpcPath client uuid req = do
  footer <- printPreamble "REQUEST" rpcPath client uuid
  unless (null $ _grpcClientHeaders client) $
    putStrLn $
      showHdrs "Headers" (_grpcClientHeaders client)
  putStrLn $ "Compression: " ++ BS.unpack (_compressionName (_grpcClientCompression client))
  putStrLn $ "Message:\n" ++ indent 1 (show req)
  putStrLn footer

simpleReplyLogger :: ReplyLogger IO
simpleReplyLogger rpcPath client uuid reply = do
  footer <- printPreamble "REPLY" rpcPath client uuid
  case reply of
    Left _clientErr -> putStrLn "Error: Early end of Stream!"
    Right (Left _tmc) -> putStrLn "Error: Too much concurrency!"
    Right (Right (Left errCode)) -> putStrLn $ "Error code: " ++ show errCode
    Right (Right (Right (hdrs, trailers, Left errMsg))) -> do
      putStr $ showHdrTrailers hdrs trailers
      putStrLn $ "Error: " ++ errMsg
    Right (Right (Right (hdrs, trailers, Right o))) -> do
      putStr $ showHdrTrailers hdrs trailers
      putStrLn $ "Message:\n" ++ indent 1 (show o)
  putStrLn footer

simpleServerStreamLogger :: ServerStreamLogger IO
simpleServerStreamLogger rpcPath client (uuid, index) o = do
  footer <- printPreamble "STREAM DATA" rpcPath client uuid
  putStrLn $ "Message (#" ++ show index ++ "):"
  putStrLn $ indent 1 (show o)
  putStrLn footer

simpleServerStreamEndLogger :: ServerStreamEndLogger IO
simpleServerStreamEndLogger rpcPath client (uuid, numMessages) (hdrs, trailers) = do
  footer <- printPreamble "STREAM DATA" rpcPath client uuid
  putStrLn $ showHdrTrailers' hdrs trailers
  putStrLn $ "Server stream ended after " ++ show numMessages ++ " messages."
  putStrLn footer

printPreamble :: String -> BS.ByteString -> GrpcClient -> UUID -> IO String
printPreamble eventName rpcPath client uuid = do
  (header, footer) <- headerFooter eventName
  putStrLn header
  putStrLn $ "UUID: " ++ show uuid
  putStrLn $ "Path: " ++ BS.unpack rpcPath
  putStrLn $ "Authority: " ++ show (_grpcClientAuthority client)
  return footer

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

showHdrTrailers :: [(CI BS.ByteString, BS.ByteString)] -> Maybe [(CI BS.ByteString, BS.ByteString)] -> String
showHdrTrailers hdrs trailers =
  showCIHdrs "Headers" hdrs
    ++ maybe "" (showCIHdrs "Trailers") trailers

showHdrTrailers' :: [(BS.ByteString, BS.ByteString)] -> [(BS.ByteString, BS.ByteString)] -> String
showHdrTrailers' hdrs trailers =
  showHdrs "Headers" hdrs
    ++ showHdrs "Trailers" trailers

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
