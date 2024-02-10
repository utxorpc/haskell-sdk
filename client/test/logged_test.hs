{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Monad (foldM)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (foldl')
import Data.ProtoLens.Message (defMessage)
import Network.GRPC.Client (HeaderList, RawReply)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Proto.Utxorpc.V1.Sync.Sync
import Test.Hspec
import Utxorpc.Logged
import "http2-client" Network.HTTP2.Client (ClientError, TooMuchConcurrency)

type UnaryReq = FetchBlockRequest

unaryReq :: UnaryReq
unaryReq = defMessage

type UnaryReply = FetchBlockResponse

type UnaryRpc = RPC ChainSyncService "fetchBlock"

unaryRpc :: UnaryRpc
unaryRpc = RPC

type StreamReq = FollowTipRequest

streamReq :: StreamReq
streamReq = defMessage

type StreamReply = FollowTipResponse

streamReplies :: Int -> [StreamReply]
streamReplies n = replicate n defMessage

type StreamRpc = RPC ChainSyncService "followTip"

streamRpc :: StreamRpc
streamRpc = RPC

mockLogger :: (String -> IO ()) -> UtxorpcClientLogger IO
mockLogger f =
  UtxorpcClientLogger
    (mockRequestLogger f)
    (mockReplyLogger f)
    (mockStreamLogger f)
    (mockStreamEndLogger f)
    id

showIndex :: Int -> String
showIndex i = " (" ++ show i ++ ")"

mockRequestLogger :: (String -> IO ()) -> RequestLogger IO
mockRequestLogger f _ _ _ msg = f $ requestLoggerOut msg

requestLoggerOut :: (Show a) => a -> String
requestLoggerOut msg = "REQUEST_LOGGER" ++ show msg

mockReplyLogger :: (String -> IO ()) -> ReplyLogger IO
mockReplyLogger f _ _ _ msg = f $ replyLoggerOut msg

replyLoggerOut ::
  (Show a) =>
  Either ClientError (Either TooMuchConcurrency (RawReply a)) ->
  String
replyLoggerOut (Right (Right (Right (_, _, Right msg)))) = "REPLY_LOGGER" ++ show msg
replyLoggerOut _ = "REPLY_LOGGER: error"

mockStreamLogger :: (String -> IO ()) -> ServerStreamLogger IO
mockStreamLogger f _ _ (_, index) msg = f $ streamLoggerOut index msg

streamLoggerOut :: (Show a) => Int -> a -> String
streamLoggerOut index msg = "STREAM_LOGGER" ++ showIndex index ++ show msg

mockStreamEndLogger :: (String -> IO ()) -> ServerStreamEndLogger IO
mockStreamEndLogger f _ _ (_, index) _ = f $ streamEndLoggerOut index

streamEndLoggerOut :: Int -> String
streamEndLoggerOut index = "STREAM_END_LOGGER" ++ showIndex index

mockUnaryExecutor :: UnaryExecutor r i UnaryReply
mockUnaryExecutor _ _ _ = return $ Right $ Right ([], Nothing, Right defMessage)

mockServerStreamExecutor :: Int -> ServerStreamExecutor r i StreamReply
mockServerStreamExecutor n _ _ initStreamState _ inStream = do
  finalStreamState <- foldM run initStreamState $ streamReplies n
  return $ Right (finalStreamState, [], [])
  where
    run state = inStream state []

mockInStream :: (String -> IO ()) -> Int -> HeaderList -> StreamReply -> IO Int
mockInStream logF index _ msg = do
  logF $ inStreamOut index msg
  return $ index + 1

inStreamOut :: (Show o) => Int -> o -> String
inStreamOut index msg = "IN_STREAM_OUT" ++ showIndex index ++ show msg

testUnary :: IO ()
testUnary = do
  out <- newIORef ""
  let logF str = writeIORef out . (++ str ++ "\n") =<< readIORef out
  _reply <-
    loggedUnary'
      mockUnaryExecutor
      (Just $ mockLogger logF)
      unaryRpc
      undefined
      unaryReq
  out' <- readIORef out

  hspec $ describe "Unary Logging" $ do
    it "should log the request" $ do
      head (lines out') `shouldBe` requestLoggerOut unaryReq
    it "should log the reply" $ do
      tail (lines out')
        `shouldBe` [ replyLoggerOut $
                       Right (Right (Right ([], Nothing, Right (defMessage :: UnaryReply))))
                   ]

testStream :: IO ()
testStream = do
  out <- newIORef ""
  let logF str = writeIORef out . (++ str ++ "\n") =<< readIORef out
  let numStreamMsgs = 3

  _finalStreamState <-
    loggedSStream'
      (mockServerStreamExecutor numStreamMsgs)
      (Just $ mockLogger logF)
      streamRpc
      undefined
      0
      streamReq
      (mockInStream logF)
  out' <- readIORef out

  hspec $ describe "Stream logging" $ do
    it "should log the request" $ do
      head (lines out') `shouldBe` requestLoggerOut streamReq
    it "should log the stream" $ do
      let msgsWithIndex = zip [0 .. numStreamMsgs] (streamReplies numStreamMsgs)
      let targetStreamLogs (index, msg) = [streamLoggerOut index msg, inStreamOut index msg]
      let f logs (index, msg) = logs ++ targetStreamLogs (index, msg)
      init (tail (lines out'))
        `shouldBe` foldl' f [] msgsWithIndex
    it "should log the end of the stream" $ do
      last (lines out') `shouldBe` streamEndLoggerOut numStreamMsgs

main :: IO ()
main = do
  testUnary
  testStream
