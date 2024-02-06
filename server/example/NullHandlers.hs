module NullHandlers (nullUnaryHandler, nullSStreamHandler) where

import Data.ProtoLens (Message (defMessage))
import Network.GRPC.Server (ServerStream (..))
import Network.Wai (Request)

nullUnaryHandler :: (Monad m, Message o) => Request -> i -> m o
nullUnaryHandler _ _ = return defMessage

nullSStreamHandler :: (Monad m, Message o) => Request -> i -> m (Int, ServerStream m o Int)
nullSStreamHandler _ _ =
  return
    ( 0,
      ServerStream $ \n ->
        if n > 3
          then return Nothing
          else return $ Just (n + 1, defMessage)
    )
