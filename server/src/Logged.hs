{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Logged
  ( UtxorpcServerLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerChunkLogger,
    ServerStreamEndLogger,
    loggedUnary,
    loggedSStream,
    simpleLogger,
  )
where

import Control.Lens
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import Data.ProtoLens (Message)
import Data.Time (getZonedTime)
import Data.UUID (UUID, nil, toString)
import Data.UUID.V4 (nextRandom)
import Network.GRPC.HTTP2.Encoding (GRPCInput, GRPCOutput)
import Network.GRPC.HTTP2.Types (IsRPC (..))
import Network.GRPC.Server (ServiceHandler, UnaryHandler)
import Network.GRPC.Server.Handlers.Trans (ServerStream (..), ServerStreamHandler, serverStream, unary)
import Network.Wai (Request (..))

data UtxorpcServerLogger m a = UtxorpcServerLogger
  { requestL :: RequestLogger m a,
    replyL :: ReplyLogger m a,
    serverChunkL :: ServerChunkLogger m a,
    serverStreamEndL :: ServerStreamEndLogger m a
  }

type RequestLogger m a =
  forall i.
  (Message i, Show i) =>
  BS.ByteString ->
  Request ->
  i ->
  m a

type ReplyLogger m a =
  forall o.
  (Message o, Show o) =>
  BS.ByteString ->
  Request ->
  a ->
  o ->
  m ()

type ServerChunkLogger m a =
  forall o.
  (Message o, Show o) =>
  BS.ByteString ->
  Request ->
  a ->
  o ->
  m a

type ServerStreamEndLogger m a = BS.ByteString -> Request -> a -> m ()

loggedUnary ::
  (MonadIO m, GRPCInput r i, GRPCOutput r o, Message i, Show i, Message o, Show o) =>
  Maybe (UtxorpcServerLogger m a) ->
  (forall x. m x -> IO x) ->
  r ->
  UnaryHandler m i o ->
  ServiceHandler
loggedUnary logger f rpc handler =
  unary f rpc $ maybe handler loggedHandler logger
  where
    loggedHandler l req i = do
      initLogState <- requestL l (path rpc) req i
      reply <- handler req i
      replyL l rpcPath req initLogState reply
      return reply
    rpcPath = path rpc

loggedSStream ::
  (MonadIO m, GRPCInput r i, GRPCOutput r o, Message i, Show i, Message o, Show o) =>
  Maybe (UtxorpcServerLogger m b) ->
  (forall x. m x -> IO x) ->
  r ->
  ServerStreamHandler m i o a ->
  --  ^ Request -> i -> m (a, ServerStream m o a)
  --      ServerStream m o a ~ ServerStream { serverStreamNext :: a -> m (Maybe (a, o)) }
  ServiceHandler
loggedSStream Nothing f rpc handler = serverStream f rpc handler
loggedSStream (Just logger) f rpc handler =
  serverStream f rpc $ loggedHandler logger
  where
    loggedHandler UtxorpcServerLogger {requestL, serverChunkL, serverStreamEndL} req i = do
      initLogState <- requestL rpcPath req i
      -- \^ Log request
      (initStreamState, ServerStream {serverStreamNext}) <- handler req i
      -- \^ Get next stream chunk generator
      let loggedStreamNext = mkLoggedStreamNext serverStreamNext
      -- \^ Wrap next stream chunk generator in logging functions
      return ((initStreamState, initLogState), ServerStream loggedStreamNext)
      where
        mkLoggedStreamNext nextChunk (streamState, logState) = do
          res <- nextChunk streamState
          -- \^ Get next chunk
          case res of
            Nothing -> do
              serverStreamEndL rpcPath req logState
              -- \^ Log end of stream
              return Nothing
            -- \^ Return end of stream
            Just (nextStreamState, msg) -> do
              nextLogState <- serverChunkL rpcPath req logState msg
              -- Log chunk
              return $ Just ((nextStreamState, nextLogState), msg)
    rpcPath = path rpc

simpleLogger :: (MonadIO m) => UtxorpcServerLogger m (UUID, Int)
simpleLogger =
  UtxorpcServerLogger
    simpleRequestLogger
    simpleReplyLogger
    (\_ _ _ _ -> liftIO $ putStrLn "Logger not implemented yet" >> return (nil, 0))
    (\_ _ _ -> liftIO $ putStrLn "Logger not implemented yet")

simpleRequestLogger :: (Show i, MonadIO m) => BS.ByteString -> Request -> i -> m (UUID, Int)
simpleRequestLogger _ req i = do
  reqId <- liftIO nextRandom
  zoned <- liftIO getZonedTime
  liftIO . putStrLn $ showRequest reqId zoned
  return (reqId, 0)
  where
    showRequest reqId zoned =
      padOpening ("Call received: " ++ show zoned)
        ++ "\n"
        ++ "Req ID: "
        ++ show reqId
        ++ "\n"
        ++ indent 1 (showRequestMeta req)
        ++ "\tMessage:\n"
        ++ indent 2 (show i)
        ++ padOpening ""

showRequestMeta :: Request -> String
showRequestMeta req =
  "Path: "
    ++ BS.unpack (rawPathInfo req)
    ++ "\nQuery string: "
    ++ BS.unpack (rawQueryString req)
    ++ "\nHeaders: "
    ++ ( if null $ requestHeaders req
           then ""
           else
             "\n"
               ++ indent 1 (showHdrs (requestHeaders req))
       )
    ++ "Is secure: "
    ++ show (isSecure req)
    ++ "\nRemote Host: "
    ++ show (remoteHost req)
  where
    showHdrs :: [(CI.CI BS.ByteString, BS.ByteString)] -> String
    showHdrs =
      unlines
        . map
          ( \h ->
              BS.unpack (CI.original $ h ^. _1) ++ ": " ++ BS.unpack (h ^. _2)
          )

simpleReplyLogger :: (Show o, MonadIO m) => BS.ByteString -> Request -> (UUID, Int) -> o -> m ()
simpleReplyLogger _ req (reqId, _) o =
  do
    zoned <- liftIO getZonedTime
    liftIO . putStrLn $ showReply zoned
  where
    showReply zoned =
      padOpening ("Reply sent: " ++ show zoned)
        ++ "\n"
        ++ "Req ID: "
        ++ toString reqId
        ++ "\n"
        ++ indent 1 (showRequestMeta req)
        ++ "\n\tMessage:\n"
        ++ indent 2 (show o)
        ++ padOpening ""

padOpening :: String -> String
padOpening = pad 75 '-'
  where
    pad :: Int -> Char -> String -> String
    pad w c s
      | w - length s <= 0 = s
      | otherwise =
          let n = (w - length s) `div` 2
              n' = n + (w - length s) `mod` 2
           in replicate n c ++ s ++ replicate n' c

indent :: Int -> String -> String
indent n = unlines . map (replicate n '\t' ++) . lines
