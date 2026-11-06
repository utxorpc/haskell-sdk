
module Main (main) where

import Control.Lens.Combinators (view)
import Control.Monad (forM_, unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BC
import Data.ProtoLens (decodeMessage, encodeMessage)
import Proto.Utxorpc.V1alpha.Cardano.Cardano (Block)
import Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields (body, hash, header, slot, tx)
import Test.Hspec

data Vector = Vector
  { name :: String,
    expectedSlot :: String,
    expectedHash :: String,
    expectedTxCount :: Int,
    expectedFirstTx :: String
  }

vectorsDir :: FilePath
vectorsDir = "test/vectors/cardano"

parseRow :: String -> Vector
parseRow line = case words (map (\c -> if c == '\t' then ' ' else c) line) of
  [n, _, _, s, h, c, f] -> Vector n s h (read c) f
  _ -> error ("bad row: " ++ line)

hex :: BS.ByteString -> String
hex = BC.unpack . B16.encode

main :: IO ()
main = do
  index <- readFile (vectorsDir ++ "/vectors.tsv")
  let vectors = map parseRow (drop 1 (lines index))
  hspec $ describe "cardano vectors" $ forM_ vectors $ \v ->
    it ("decodes " ++ name v) $ do
      raw <- BS.readFile (vectorsDir ++ "/v1alpha/" ++ name v ++ ".pb")
      block <- either fail pure (decodeMessage raw) :: IO Block
      show (view (header . slot) block) `shouldBe` expectedSlot v
      hex (view (header . hash) block) `shouldBe` expectedHash v
      let txs = view (body . tx) block
      length txs `shouldBe` expectedTxCount v
      unless (expectedFirstTx v == "-") $
        hex (view hash (head txs)) `shouldBe` expectedFirstTx v
      encodeMessage block `shouldBe` raw
