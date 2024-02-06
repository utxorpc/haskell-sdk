module Main where

import Control.Exception (throwIO)
import Control.Lens.Operators ((&), (.~))
import qualified Data.ByteString.Char8 as BS
import Data.ProtoLens (Message (..))
import Proto.Utxorpc.V1.Sync.Sync_Fields (hash, index, ref)
import UnliftIO.Exception (throwString)
import Utxorpc.Client (simpleUtxorpcService)
import Utxorpc.Types (fetchBlock, syncS)

main :: IO ()
main = do
  -- Connect to a UTxO RPC service
  eService <- simpleUtxorpcService "hostname" 443 True
  case eService of
    Left clientErr -> throwIO clientErr
    Right service -> do
      -- Make a unary request
      result <- fetchBlock (syncS service) fetchBlockRequest
      case result of
        -- Handle success
        Right (Right (Right (_headers, _trailers, fetchBlockResponse))) ->
          print fetchBlockResponse
        error -> throwString $ show error
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
