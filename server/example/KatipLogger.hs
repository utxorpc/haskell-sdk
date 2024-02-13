{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module KatipLogger (katipLogger, unliftKatip, mkLogEnv) where

import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import Data.UUID (UUID)
import Katip
import Katip.Monadic
import UnliftIO (stdout)
import Utxorpc.Server (ReplyLogger, RequestLogger, ServerStreamEndLogger, ServerStreamLogger, UtxorpcServiceLogger (..))

katipLogger :: (KatipContext m) => UtxorpcServiceLogger m
katipLogger =
  UtxorpcServiceLogger
    katipRequestLogger
    katipReplyLogger
    katipServerStreamLogger
    katipServerStreamEndLogger

unliftKatip :: LogEnv -> KatipContextT IO a -> IO a
unliftKatip le ctxT = do
  let rdr = unKatipContextT ctxT
  let initState = KatipContextTState le mempty mempty
  runReaderT rdr initState

mkLogEnv :: IO LogEnv
mkLogEnv = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
  le <- initLogEnv "UtxorpcService" "production"
  registerScribe "stdout" handleScribe defaultScribeSettings le

katipRequestLogger :: (KatipContext m) => RequestLogger m
katipRequestLogger rpcPath _req uuid i = do
  addContext rpcPath uuid Nothing Request $
    $(logTM) InfoS (showLS i)
  return ()

katipReplyLogger :: (KatipContext m) => ReplyLogger m
katipReplyLogger path _req uuid reply = do
  addContext path uuid Nothing Reply $
    $(logTM) InfoS (showLS reply)

katipServerStreamLogger :: (KatipContext m) => ServerStreamLogger m
katipServerStreamLogger path _req (uuid, index) msg =
  addContext path uuid (Just index) ServerStream $
    $(logTM) InfoS (showLS msg)

katipServerStreamEndLogger :: (KatipContext m) => ServerStreamEndLogger m
katipServerStreamEndLogger path _req (uuid, index) =
  addContext path uuid (Just index) ServerStreamEnd $
    $(logTM) InfoS "End of stream"

addContext :: (KatipContext m) => BS.ByteString -> UUID -> Maybe Int -> GrpcEvent -> m a -> m a
addContext path uuid index event = katipAddContext $ MsgContext path uuid index event

data MsgContext = MsgContext
  { _path :: BS.ByteString,
    _uuid :: UUID,
    _index :: Maybe Int,
    _event :: GrpcEvent
  }

data GrpcEvent = Request | Reply | ServerStream | ServerStreamEnd deriving (Show)

instance ToJSON MsgContext where
  toJSON :: MsgContext -> Value
  toJSON MsgContext {_path, _uuid, _index, _event} =
    A.object
      ( "path"
          A..= BS.unpack _path
          : "UUID"
          A..= show _uuid
          : "Event"
          A..= show _event
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
