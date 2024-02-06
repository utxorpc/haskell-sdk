{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Utxorpc.Logged
  ( UtxorpcClientLogger (..),
    RequestLogger,
    ReplyLogger,
    ServerStreamDataLogger,
    ServerStreamEndLogger,
    loggedSStream,
    loggedUnary,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (Message)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.GRPC.Client (HeaderList, RawReply)
import Network.GRPC.Client.Helpers (GrpcClient (..), rawStreamServer, rawUnary)
import Network.GRPC.HTTP2.Encoding (GRPCInput, GRPCOutput)
import Network.GRPC.HTTP2.Types (IsRPC (..))
import Network.HTTP2.Client (ClientError, TooMuchConcurrency, runClientIO)
import Utxorpc.Types (ServerStreamReply, UnaryReply)

{--------------------------------------
  Types
--------------------------------------}

-- In the following logger types:
--
-- The ByteString is the method path
--
-- The GrcpClient is included because it contains useful information such as the server
-- address
--
-- A UUID is generated for each call, and server stream methods receive the index of the message
-- (i.e., how many messages have already been received in the current stream).
data UtxorpcClientLogger m = UtxorpcClientLogger
  { requestL :: RequestLogger m,
    replyL :: ReplyLogger m,
    serverStreamDataL :: ServerStreamDataLogger m,
    serverStreamEndL :: ServerStreamEndLogger m,
    unlift :: forall x. m x -> IO x
  }

type RequestLogger m =
  forall i.
  (Show i, Message i) =>
  BS.ByteString ->
  GrpcClient ->
  UUID ->
  i -> -- Message sent to the service
  m ()

type ReplyLogger m =
  forall o.
  (Show o, Message o) =>
  BS.ByteString ->
  GrpcClient ->
  UUID ->
  Either ClientError (Either TooMuchConcurrency (RawReply o)) -> -- Message received from the service (with headers)
  m ()

type ServerStreamDataLogger m =
  forall o.
  (Show o, Message o) =>
  BS.ByteString ->
  GrpcClient ->
  (UUID, Int) ->
  o -> -- Message received from the service
  m ()

type ServerStreamEndLogger m =
  BS.ByteString ->
  GrpcClient ->
  (UUID, Int) ->
  (HeaderList, HeaderList) -> -- Headers and Trailers
  m ()

{--------------------------------------
  Logged wrappers of gRPC library functions
--------------------------------------}

loggedUnary ::
  (GRPCInput r i, GRPCOutput r o, Show i, Message i, Show o, Message o) =>
  Maybe (UtxorpcClientLogger m) ->
  r ->
  GrpcClient ->
  i ->
  UnaryReply o
loggedUnary logger msg client i =
  maybe (runClientIO $ rawUnary msg client i) logged logger
  where
    logged UtxorpcClientLogger {requestL, replyL, unlift} = do
      uuid <- nextRandom
      unlift $ requestL (path msg) client uuid i
      o <- runClientIO $ rawUnary msg client i
      unlift $ replyL (path msg) client uuid o
      return o

-- The gRPC library requires a handler that produces a `ClientIO`,
-- but this does not make sense since a user-provided handler is not
-- likely to generate a `ClientError` or `TooMuchConcurrency`.
-- Instead, we accept a handler of type `IO` and lift it.
loggedSStream ::
  (GRPCOutput r o, GRPCInput r i, Show i, Message i, Show o, Message o) =>
  Maybe (UtxorpcClientLogger m) ->
  r ->
  GrpcClient ->
  a ->
  i ->
  (a -> HeaderList -> o -> IO a) ->
  ServerStreamReply a
loggedSStream logger r client initStreamState req chunkHandler =
  maybe
    (runClientIO $ rawStreamServer r client initStreamState req liftedChunkHandler)
    logged
    logger
  where
    liftedChunkHandler streamState headerList reply = liftIO $ chunkHandler streamState headerList reply

    logged
      UtxorpcClientLogger {requestL, serverStreamDataL, serverStreamEndL, unlift} = do
        uuid <- nextRandom
        unlift $ requestL rpcPath client uuid req
        streamResult <- runLoggedStream uuid
        handleStreamResult streamResult
        where
          runLoggedStream uuid =
            runClientIO $
              rawStreamServer r client (initStreamState, uuid, 0) req loggedChunkHandler

          loggedChunkHandler (streamState, uuid, index) headerList msg = do
            liftIO . unlift $ serverStreamDataL rpcPath client (uuid, index) msg
            a <- liftIO $ chunkHandler streamState headerList msg
            return (a, uuid, index + 1)

          handleStreamResult streamResult =
            case streamResult of
              Right (Right ((finalStreamState, uuid, index), hl, hl')) -> do
                liftIO . unlift $ serverStreamEndL rpcPath client (uuid, index) (hl, hl')
                return $ Right $ Right (finalStreamState, hl, hl')
              Right (Left tmc) -> return $ Right $ Left tmc
              Left errCode -> return $ Left errCode

          rpcPath = path r
