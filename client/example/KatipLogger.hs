{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KatipLogger (katipLogger) where

import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.UUID (UUID)
import Katip
import Katip.Monadic (KatipContextTState (..), unKatipContextT)
import Utxorpc.Client

katipLogger :: KatipContextTState -> UtxorpcClientLogger (KatipContextT IO)
katipLogger st =
  UtxorpcClientLogger
    katipRequestLogger
    katipReplyLogger
    katipServerStreamLogger
    katipServerStreamEndLogger
    (\action -> runReaderT (unKatipContextT action) st)

katipRequestLogger :: RequestLogger (KatipContextT IO)
katipRequestLogger path _client uuid msg =
  addPathContext path uuid Nothing $
    $(logTM) InfoS (showLS msg)

katipReplyLogger :: ReplyLogger (KatipContextT IO)
katipReplyLogger path _client uuid reply =
  addPathContext path uuid Nothing $
    $(logTM) InfoS (showLS reply)

katipServerStreamLogger :: ServerStreamLogger (KatipContextT IO)
katipServerStreamLogger path _client (uuid, index) msg =
  addPathContext path uuid (Just index) $
    $(logTM) InfoS (showLS msg)

katipServerStreamEndLogger :: ServerStreamEndLogger (KatipContextT IO)
katipServerStreamEndLogger path _client (uuid, index) (_headers, _trailers) =
  addPathContext path uuid (Just index) $
    $(logTM) InfoS "End of stream"

addPathContext :: (KatipContext m) => BS.ByteString -> UUID -> Maybe Int -> m a -> m a
addPathContext path uuid index = katipAddContext $ MsgContext path uuid index

data MsgContext = MsgContext
  { _path :: BS.ByteString,
    _uuid :: UUID,
    _index :: Maybe Int
  }

instance ToJSON MsgContext where
  toJSON :: MsgContext -> Value
  toJSON MsgContext {_path, _uuid, _index} =
    A.object
      ( "path"
          A..= BS.unpack _path
          : "UUID"
          A..= show _uuid
          : indexContext
      )
    where
      indexContext = case _index of
        Just idx -> ["Message #" A..= show idx]
        Nothing -> []

instance ToObject MsgContext

instance LogItem MsgContext where
  payloadKeys :: Verbosity -> MsgContext -> PayloadSelection
  payloadKeys _ _ = AllKeys
