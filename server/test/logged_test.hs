{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, tell)
import Data.List (foldl')
import Data.ProtoLens.Message (Message, defMessage)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Network.GRPC.HTTP2.ProtoLens (RPC (RPC))
import Network.GRPC.Server (ServerStream (..), ServerStreamHandler, UnaryHandler)
import Proto.Utxorpc.V1.Sync.Sync
import System.Directory (removeFile)
import System.IO (Handle, IOMode (..), hPutStr, readFile', withFile)
import Test.Hspec
import Utxorpc.Logged
  ( ReplyLogger,
    RequestLogger,
    ServerStreamEndLogger,
    ServerStreamLogger,
    UtxorpcServiceLogger (..),
    loggedSStreamHandler,
    loggedUnaryHandler,
  )

logFilePath :: String
logFilePath = "test/test-logs.txt"

mockRequestLogger :: (String -> IO ()) -> RequestLogger (WriterT String IO)
mockRequestLogger logF _ _ uuid msg = do
  lift . logF $ requestOut uuid msg
  tell $ requestOut uuid msg

requestOut :: (Show a) => UUID -> a -> String
requestOut uuid msg = "REQUEST_LOGGER: " ++ "[" ++ show uuid ++ "]" ++ show msg ++ "\n"

mockReplyLogger :: (String -> IO ()) -> ReplyLogger (WriterT String IO)
mockReplyLogger logF _ _ uuid msg = do
  lift . logF $ replyOut uuid msg
  tell $ replyOut uuid msg

replyOut :: (Show a) => UUID -> a -> String
replyOut uuid msg = "REPLY_LOGGER: [" ++ show uuid ++ "] " ++ show msg ++ "\n"

mockServerStreamLogger :: (String -> IO ()) -> ServerStreamLogger (WriterT String IO)
mockServerStreamLogger logF _ _ (uuid, index) _ = do
  lift . logF $ serverStreamOut (uuid, index)
  tell $ serverStreamOut (uuid, index)

serverStreamOut :: (UUID, Int) -> String
serverStreamOut (uuid, int) = "SERVER_STREAM_LOGGER: " ++ show (uuid, int) ++ "\n"

mockServerStreamEndLogger :: (String -> IO ()) -> ServerStreamEndLogger (WriterT String IO)
mockServerStreamEndLogger logF _ _ (uuid, index) = do
  lift . logF $ serverStreamEndOut (uuid, index)
  tell $ serverStreamEndOut (uuid, index)

serverStreamEndOut :: (UUID, Int) -> String
serverStreamEndOut (uuid, index) = "SERVER_STREAM_END_LOGGER: " ++ show (uuid, index) ++ "\n"

mockLogger :: (String -> IO ()) -> UtxorpcServiceLogger (WriterT String IO)
mockLogger logF =
  UtxorpcServiceLogger
    (mockRequestLogger logF)
    (mockReplyLogger logF)
    (mockServerStreamLogger logF)
    (mockServerStreamEndLogger logF)

mockUnaryHandler :: (Message o) => (String -> IO ()) -> UnaryHandler (WriterT String IO) i o
mockUnaryHandler logF _ _ = do
  lift $ logF unaryHandlerOut
  tell unaryHandlerOut
  return defMessage

unaryHandlerOut :: String
unaryHandlerOut = "UNARY_HANDLER\n"

unaryRpc :: RPC ChainSyncService "fetchBlock"
unaryRpc = RPC

unaryReq :: FetchBlockRequest
unaryReq = defMessage

type UnaryReply = FetchBlockResponse

mockSStreamHandler :: (Message o) => (String -> IO ()) -> ServerStreamHandler (WriterT String IO) i o Int
mockSStreamHandler logF _ _ = do
  lift $ logF generateStreamOut
  tell generateStreamOut
  return (0, stream)
  where
    stream = ServerStream $ \index ->
      if index > 3
        then do
          lift $ logF $ streamHandlerEndOut index
          tell $ streamHandlerEndOut index
          return Nothing
        else do
          lift $ logF $ streamHandlerOut index
          tell $ streamHandlerOut index
          return $ Just (index + 1, defMessage)

generateStreamOut :: String
generateStreamOut = "GENERATE_STREAM\n"

streamHandlerOut :: Int -> String
streamHandlerOut index = "STREAM_HANDLER " ++ show index ++ "\n"

streamHandlerEndOut :: Int -> String
streamHandlerEndOut index = "STREAM_HANDLER_END " ++ show index ++ "\n"

sStreamRpc :: RPC ChainSyncService "followTip"
sStreamRpc = RPC

type SStreamReq = FollowTipRequest

sStreamReq :: SStreamReq
sStreamReq = defMessage

type SStreamReply = FollowTipResponse

testUnary :: IO ()
testUnary = do
  uuid <- nextRandom
  (reply, _mState) <- withFile logFilePath WriteMode (runWriterT . handleReq)
  out <- readFile' logFilePath

  (loggedReply, loggedMState) <- withFile logFilePath WriteMode (runWriterT . handleReqLogged uuid)
  loggedOut <- readFile' logFilePath

  hspec $ describe "Unary Logging" $ do
    it "Logged reply should equal unlogged reply" $ do
      reply `shouldBe` loggedReply
    -- We know that the IO effects are accumulating, so if the WriterT state equals the IO effect,
    -- we know the monadic state was passed from one logger/handler to the next
    it "Monad state should accumulate" $ do
      loggedOut `shouldBe` loggedMState
    it "Execution should occur in the right order" $ do
      targetLoggedOut uuid out reply `shouldBe` loggedOut

  removeFile logFilePath
  where
    handleReq :: Handle -> WriterT String IO UnaryReply
    handleReq fileHandle = mockUnaryHandler (hPutStr fileHandle) undefined unaryReq

    handleReqLogged :: UUID -> Handle -> WriterT String IO UnaryReply
    handleReqLogged uuid fileHandle = loggedHandler uuid (hPutStr fileHandle) undefined unaryReq
    loggedHandler uuid logF = loggedUnaryHandler unaryRpc (mockUnaryHandler logF) uuid (mockLogger logF)

    targetLoggedOut uuid unloggedOut reply =
      requestOut uuid unaryReq ++ unloggedOut ++ replyOut uuid reply

testSStream :: IO ()
testSStream = do
  -- Run unlogged
  let withLogFile = withFile logFilePath WriteMode
  ((_streamStates, msgs), _monadState) <-
    withLogFile $ runWriterT . run . handlerWith
  _out <- readFile' logFilePath

  -- Run logged
  uuid <- nextRandom
  ((loggedStreamStates, loggedMsgs), loggedMonadState) <-
    withLogFile $ runWriterT . run . loggedHandlerWith uuid
  loggedOut <- readFile' logFilePath

  hspec $ describe "Stream Logging" $ do
    it "Logged messages should equal unlogged messages" $ do
      msgs `shouldBe` loggedMsgs
    -- We know that the IO effects are accumulating, so if the WriterT state equals the IO effect,
    -- we know the monadic state was passed from one logger/handler to the next
    it "Monad state should accumulate" $ do
      loggedOut `shouldBe` loggedMonadState
    it "Execution should occur in the right order" $ do
      targetLoggedOut uuid loggedStreamStates `shouldBe` loggedOut

  removeFile logFilePath
  where
    handlerWith ::
      Handle ->
      ServerStreamHandler (WriterT String IO) SStreamReq SStreamReply Int
    handlerWith = mockSStreamHandler . hPutStr

    loggedHandlerWith ::
      UUID ->
      Handle ->
      ServerStreamHandler (WriterT String IO) SStreamReq SStreamReply (Int, Int)
    loggedHandlerWith uuid h =
      loggedSStreamHandler
        sStreamRpc
        (mockSStreamHandler $ hPutStr h)
        uuid
        (mockLogger $ hPutStr h)

    run :: (Monad m) => ServerStreamHandler m SStreamReq SStreamReply a -> m ([a], [SStreamReply])
    run handler = do
      (initState, ServerStream {serverStreamNext}) <- handler undefined sStreamReq
      runStream serverStreamNext initState
      where
        runStream stream state = do
          next <- stream state
          case next of
            Just (state', msg) -> do
              (states, msgs) <- runStream stream state'
              return (state : states, msg : msgs)
            Nothing ->
              return ([], [])

    targetLoggedOut uuid loggedStreamStates =
      requestOut uuid unaryReq -- log request
        ++ generateStreamOut -- generate stream
        -- Output for all stream handling and logs
        ++ foldl' (\out streamState -> out ++ targetOut streamState) "" loggedStreamStates
        -- end of stream handler
        ++ streamHandlerEndOut (length loggedStreamStates)
        -- end of stream logger
        ++ serverStreamEndOut (uuid, length loggedStreamStates)
      where
        -- output for a single stream message
        targetOut (index, _) =
          streamHandlerOut index
            ++ serverStreamOut (uuid, index)

main :: IO ()
main = do
  testUnary
  testSStream
