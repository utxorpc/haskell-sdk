module Main (main) where

import Control.Exception (throwIO)
import Control.Lens.Operators ((&), (.~))
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (Message (..))
import Proto.Utxorpc.V1alpha.Sync.Sync_Fields (hash, index, ref)
import UnliftIO.Exception (throwString)
import Utxorpc.Client (simpleUtxorpcClient)
import Utxorpc.Types (fetchBlock, syncClient)

main :: IO ()
main = do
  -- Connect to a UTxO RPC service
  eClient <- simpleUtxorpcClient "hostname" 443 True
  case eClient of
    Left clientErr -> throwIO clientErr
    Right client -> do
      -- Make a unary request
      result <- fetchBlock (syncClient client) fetchBlockRequest
      case result of
        -- Handle success
        Right (Right (Right (_headers, _trailers, fetchBlockResponse))) ->
          print fetchBlockResponse
        err -> throwString $ show err
  where
    fetchBlockRequest =
      defMessage
        & ref
          .~ [ defMessage
                 & index .~ 40608434
                 & hash
                   .~ BS.pack
                     "3e4947072df1ed22a0518cb717c2904ebf0952b0c33292b402fae25d9562022e"
             ]
