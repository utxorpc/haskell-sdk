module EmptyHandlers (emptyUnaryHandler, emptySStreamHandler) where

import Data.ProtoLens (Message (defMessage))
import Network.GRPC.Server (ServerStream (..))
import Network.Wai (Request)

emptyUnaryHandler :: (Monad m, Message o) => (String -> m ()) -> Request -> i -> m o
emptyUnaryHandler logF _ _ = do
  logF "A unary handler is processing a request and generating a reply..."
  return defMessage

emptySStreamHandler ::
  (Monad m, Message o) =>
  (String -> m ()) ->
  Request ->
  i ->
  m (Int, ServerStream m o Int)
emptySStreamHandler logF _ _ =
  return
    ( 0,
      ServerStream $ \n ->
        if n > 3
          then do
            logF "The stream handler is ending the stream"
            return Nothing
          else do
            logF "The stream handler is generating a message"
            return $ Just (n + 1, defMessage)
    )
