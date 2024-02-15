{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (bracket, throwIO)
import Control.Lens.Operators ((&), (.~))
import Control.Monad (void)
import Data.ProtoLens (Message (..))
import qualified Data.Text.Encoding as T
import Proto.Utxorpc.V1alpha.Sync.Sync (FetchBlockRequest)
import Proto.Utxorpc.V1alpha.Sync.Sync_Fields (hash, index, ref)
import UnliftIO.Exception (throwString)
import Utxorpc.Client (close, fetchBlock, simpleUtxorpcClient, syncClient)

main :: IO ()
main = do
  -- Connect to a UTxO RPC service
  let mkClient = simpleUtxorpcClient "127.0.0.1" 3000 True
  -- Bracket making a request with closing the client connection
  bracket mkClient closeClient $ \case
    -- Panic if connection could not be established
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
    fetchBlockRequest :: FetchBlockRequest
    fetchBlockRequest =
      defMessage
        & ref
          .~ [ defMessage
                 & index .~ 40608434
                 & hash
                   .~ T.encodeUtf8
                     "3e4947072df1ed22a0518cb717c2904ebf0952b0c33292b402fae25d9562022e"
             ]

    -- Close the client connection if it was established
    closeClient (Left _) = return ()
    closeClient (Right client) = void (close client)
