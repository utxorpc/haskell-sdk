{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Main (main) where

import Control.Exception (bracket, throwIO, try, SomeException)
import Control.Lens ((^.), (.~), (&))
import Control.Monad (void, when, forM_)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.ProtoLens (Message (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Int (Int64)
import Data.List (find)
-- import Data.Maybe (isJust, fromMaybe)
-- import Proto.Utxorpc.V1alpha.Query.Query
import qualified Proto.Utxorpc.V1alpha.Query.Query_Fields as QF
-- import Proto.Utxorpc.V1alpha.Cardano.Cardano
import qualified Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields as CF
import Proto.Utxorpc.V1alpha.Sync.Sync (FollowTipResponse)
import qualified Proto.Utxorpc.V1alpha.Sync.Sync as S
import qualified Proto.Utxorpc.V1alpha.Sync.Sync_Fields as SF
import Proto.Utxorpc.V1alpha.Submit.Submit (WaitForTxResponse, WatchMempoolResponse, Stage(..), TxPredicate, AnyChainTxPattern)
import qualified Proto.Utxorpc.V1alpha.Submit.Submit as Sub
import qualified Proto.Utxorpc.V1alpha.Submit.Submit_Fields as SubF
import Proto.Utxorpc.V1alpha.Watch.Watch (WatchTxResponse, WatchTxResponse'Action(..))
import qualified Proto.Utxorpc.V1alpha.Watch.Watch as W
import qualified Proto.Utxorpc.V1alpha.Watch.Watch_Fields as WF
import Proto.Utxorpc.V1alpha.Cardano.Cardano (AddressPattern, AssetPattern, TxPattern)
import qualified Proto.Utxorpc.V1alpha.Cardano.Cardano as C
import UnliftIO.Exception (throwString)
import GHC.Stack (HasCallStack)
import Utxorpc.Client

-- Test helpers
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual msg expected actual =
  when (expected /= actual) $
    throwString $ msg ++ ": expected " ++ show expected ++ ", got " ++ show actual

assertNotNull :: HasCallStack => String -> Maybe a -> IO ()
assertNotNull msg Nothing = throwString $ msg ++ " is null"
assertNotNull _ (Just _) = return ()

assertTrue :: HasCallStack => String -> Bool -> IO ()

-- BigInt helper - extracts Int64 from BigInt protobuf message
getBigInt :: C.BigInt -> Int64
getBigInt bigIntMsg = case bigIntMsg ^. CF.maybe'bigInt of
  Just (C.BigInt'Int val) -> val
  _ -> error "Expected BigInt'Int variant"
assertTrue msg False = throwString $ msg ++ " is false"
assertTrue _ True = return ()

assertSingle :: HasCallStack => String -> [a] -> IO a
assertSingle msg [] = throwString $ msg ++ " is empty"
assertSingle _ [x] = return x
assertSingle msg xs = throwString $ msg ++ " has " ++ show (length xs) ++ " items, expected 1"

main :: IO ()
main = do
  putStrLn "Testing Query and Submit endpoints with Dolos"

  -- Connect to Dolos UTxO RPC service
  let mkClient = simpleUtxorpcClient "127.0.0.1" 50051 False

  bracket mkClient closeClient $ \case
    Left clientErr -> throwIO clientErr
    Right client -> do
      -- Query tests
      testReadParams client
      testReadUtxosByOutputRef client
      testSearchUtxosByAddress client
      testSearchUtxosByPaymentPart client
      testSearchUtxosByDelegationPart client
      testSearchUtxosByPolicyID client
      testSearchUtxosByAsset client

      -- Sync tests
      testSyncFollowTip client
      testSyncReadTip client
      testSyncFetchBlock client
      testSyncDumpHistory client

      -- Submit tests
      testSubmitTx client
      testWaitForTx client

      -- WatchMempool tests
      -- Submit transaction once for all WatchMempool tests
      putStrLn "\n=== Submitting transaction for WatchMempool tests ==="
      let txCborHex = "84A300D9010282825820F3627A04A2C0020720ACBD47E8CC90A1D64E9DAC9FCA7F12EB78DB8174E1FBE601825820B8BE6DF6FE61C465ED957D1901BC095C03E31612D1BC70EBFCC05CAC984A0088000182A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011A005B8D80A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011B00000001D40D1E15021A00029E8DA100D9010281825820D504D361777F985265A6F0A727E601E70F837574652760456B41731FF4F952585840191 02F26AF35BF37D9B220F71D68CACF28C596F6F74C45C1732820B61F3CBAED991C2D20E61FA38179AC228A1673B670F32544015D91AF74D786AA24C07CE900F5F6"
          txCborBytes = case Base16.decode (T.encodeUtf8 $ T.pack $ filter (/= ' ') txCborHex) of
            Right bs -> bs
            Left err -> error $ "Invalid CBOR hex: " ++ err
          anyChainTx = defMessage & SubF.raw .~ txCborBytes
          submitRequest = defMessage & SubF.tx .~ anyChainTx

      submitResult <- submitTx (submitClient client) submitRequest
      case submitResult of
        Right (Right (Right (_headers, _trailers, Right response))) -> do
          let txHash = response ^. SubF.ref
          if not (BS.null txHash) then do
            putStrLn $ "  Transaction submitted! Hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txHash) ++ "..."

            -- Now run all WatchMempool tests - they'll all detect this transaction
            testWatchMempoolByAddress client
            testWatchMempoolByPaymentPart client
            testWatchMempoolByDelegationPart client
            testWatchMempoolByPolicyId client
            testWatchMempoolByAsset client
          else
            throwString "Transaction submission returned no refs"
        err -> throwString $ "Failed to submit transaction: " ++ show err

      -- Watch tests - each test submits its own transaction
      putStrLn "\n=== Running Watch tests (each submits and watches for blockchain application) ==="
      testWatchTxForAddress client
      testWatchTxForPaymentPart client
      testWatchTxForDelegationPart client
      testWatchTxForPolicyId client
      testWatchTxForAsset client

      putStrLn "\n✅ All tests passed!"
  where
    closeClient (Left _) = return ()
    closeClient (Right client) = void (close client)

testReadParams :: UtxorpcClient -> IO ()
testReadParams client = do
  putStrLn "\n=== Test: ReadParams ==="

  result <- readParams (queryClient client) defMessage
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Get the parameters
      let params = response ^. QF.values . QF.cardano

      -- Show some actual values from the response
      putStrLn $ "  coinsPerUtxoByte: " ++ show (params ^. CF.coinsPerUtxoByte)
      putStrLn $ "  maxTxSize: " ++ show (params ^. CF.maxTxSize)
      putStrLn $ "  minFeeCoefficient: " ++ show (params ^. CF.minFeeCoefficient)
      putStrLn $ "  protocolVersion: " ++ show (params ^. CF.protocolVersion . CF.major)

      -- Assert all expected parameter values for Preview testnet
      -- BigInt fields need to be accessed via their bigInt oneof field using getBigInt helper
      assertEqual "coinsPerUtxoByte" 4310 (getBigInt $ params ^. CF.coinsPerUtxoByte)
      assertEqual "maxTxSize" 16384 (params ^. CF.maxTxSize)
      assertEqual "minFeeCoefficient" 44 (getBigInt $ params ^. CF.minFeeCoefficient)
      assertEqual "minFeeConstant" 155381 (getBigInt $ params ^. CF.minFeeConstant)
      assertEqual "maxBlockBodySize" 90112 (params ^. CF.maxBlockBodySize)
      assertEqual "maxBlockHeaderSize" 1100 (params ^. CF.maxBlockHeaderSize)
      assertEqual "stakeKeyDeposit" 2000000 (getBigInt $ params ^. CF.stakeKeyDeposit)
      assertEqual "poolDeposit" 500000000 (getBigInt $ params ^. CF.poolDeposit)
      assertEqual "desiredNumberOfPools" 500 (params ^. CF.desiredNumberOfPools)
      assertEqual "minPoolCost" 170000000 (getBigInt $ params ^. CF.minPoolCost)
      assertEqual "maxValueSize" 5000 (params ^. CF.maxValueSize)
      assertEqual "collateralPercentage" 150 (params ^. CF.collateralPercentage)
      assertEqual "maxCollateralInputs" 3 (params ^. CF.maxCollateralInputs)

      -- Protocol version
      assertEqual "protocolVersion.major" 9 (params ^. CF.protocolVersion . CF.major)
      
      -- Rational parameters
      assertEqual "poolInfluence.numerator" 5033165 (params ^. CF.poolInfluence . CF.numerator)
      assertEqual "poolInfluence.denominator" 16777216 (params ^. CF.poolInfluence . CF.denominator)
      
      assertEqual "monetaryExpansion.numerator" 6442451 (params ^. CF.monetaryExpansion . CF.numerator)
      assertEqual "monetaryExpansion.denominator" 2147483648 (params ^. CF.monetaryExpansion . CF.denominator)
      
      assertEqual "treasuryExpansion.numerator" 13421773 (params ^. CF.treasuryExpansion . CF.numerator)
      assertEqual "treasuryExpansion.denominator" 67108864 (params ^. CF.treasuryExpansion . CF.denominator)
      
      -- Prices
      assertEqual "prices.steps.numerator" 721 (params ^. CF.prices . CF.steps . CF.numerator)
      assertEqual "prices.steps.denominator" 10000000 (params ^. CF.prices . CF.steps . CF.denominator)
      
      assertEqual "prices.memory.numerator" 577 (params ^. CF.prices . CF.memory . CF.numerator)
      assertEqual "prices.memory.denominator" 10000 (params ^. CF.prices . CF.memory . CF.denominator)
      
      -- Execution units
      assertEqual "maxExecutionUnitsPerTransaction.steps" 10000000000 
        (params ^. CF.maxExecutionUnitsPerTransaction . CF.steps)
      assertEqual "maxExecutionUnitsPerTransaction.memory" 14000000 
        (params ^. CF.maxExecutionUnitsPerTransaction . CF.memory)
      
      assertEqual "maxExecutionUnitsPerBlock.steps" 20000000000 
        (params ^. CF.maxExecutionUnitsPerBlock . CF.steps)
      assertEqual "maxExecutionUnitsPerBlock.memory" 62000000 
        (params ^. CF.maxExecutionUnitsPerBlock . CF.memory)
      
      -- Governance parameters
      assertEqual "committeeTermLimit" 365 (params ^. CF.committeeTermLimit)
      assertEqual "governanceActionValidityPeriod" 30 (params ^. CF.governanceActionValidityPeriod)
      assertEqual "governanceActionDeposit" 100000000000 (getBigInt $ params ^. CF.governanceActionDeposit)
      assertEqual "drepDeposit" 500000000 (getBigInt $ params ^. CF.drepDeposit)
      assertEqual "drepInactivityPeriod" 20 (params ^. CF.drepInactivityPeriod)
      
      -- Voting thresholds
      let poolThresholds = params ^. CF.poolVotingThresholds . CF.thresholds
      assertEqual "poolVotingThresholds count" 5 (length poolThresholds)
      forM_ poolThresholds $ \threshold -> do
        assertEqual "poolThreshold.numerator" 51 (threshold ^. CF.numerator)
        assertEqual "poolThreshold.denominator" 100 (threshold ^. CF.denominator)
      
      let drepThresholds = params ^. CF.drepVotingThresholds . CF.thresholds
      assertEqual "drepVotingThresholds count" 10 (length drepThresholds)
      
      -- Cost models
      let plutusV1Values = params ^. CF.costModels . CF.plutusV1 . CF.values
      when (100788 `notElem` plutusV1Values) $ throwString "plutusV1 missing 100788"
      when (420 `notElem` plutusV1Values) $ throwString "plutusV1 missing 420"  
      when (1 `notElem` plutusV1Values) $ throwString "plutusV1 missing 1"
      when (1000 `notElem` plutusV1Values) $ throwString "plutusV1 missing 1000"
      
      putStrLn "✓ ReadParams test passed!"
      
    err -> throwString $ "ReadParams failed: " ++ show err

testReadUtxosByOutputRef :: UtxorpcClient -> IO ()
testReadUtxosByOutputRef client = do
  putStrLn "\n=== Test: ReadUtxosByOutputRef ==="

  -- Arrange
  let txHashHex = "9874bdf4ad47b2d30a2146fc4ba1f94859e58e772683e75001aca6e85de7690d"
      txHash = case Base16.decode txHashHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      outputIndex = 0
      txoRef = defMessage
        & CF.hash .~ txHash
        & CF.index .~ outputIndex
      request = defMessage
        & QF.keys .~ [txoRef]

  -- Act
  result <- readUtxos (queryClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert
      let items = response ^. QF.items
      utxo <- assertSingle "utxos" items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)

      -- Show actual data from response
      let output = utxo ^. QF.cardano
      putStrLn $ "  UTXO coin value: " ++ show (output ^. CF.coin) ++ " lovelace"
      putStrLn $ "  TxHash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txHash) ++ "..."

      -- Verify the UTXO reference matches what we requested
      assertEqual "txHash" txHash (utxo ^. QF.txoRef . CF.hash)
      assertEqual "outputIndex" outputIndex (utxo ^. QF.txoRef . CF.index)
      
      -- Verify native bytes
      let expectedNativeBytes = "82583900729c67d0de8cde3c0afc768fb0fcb1596e8cfcbf781b553efcd228813b7bb577937983e016d4e8429ff48cf386d6818883f9e88b62a804e01a05f5e100"
      let actualNativeBytesHex = Base16.encode (utxo ^. QF.nativeBytes)
      assertEqual "nativeBytes" (T.toLower $ T.pack expectedNativeBytes) (T.decodeUtf8 actualNativeBytesHex)
      
      -- Verify parsed state
      let output = utxo ^. QF.cardano
      assertTrue "coin > 0" ((getBigInt $ output ^. CF.coin) > 0)
      putStrLn "✓ ReadUtxosByOutputRef test passed!"
        
    err -> throwString $ "ReadUtxosByOutputRef failed: " ++ show err

testSearchUtxosByAddress :: UtxorpcClient -> IO ()
testSearchUtxosByAddress client = do
  putStrLn "\n=== Test: SearchUtxosByAddress ==="

  -- Arrange
  let testAddressHex = "0053fbfffab7b001281917de77f18a8087413be03401db4aa2a7dbf0ae1591d34d5b4b2728d04a80fdd041bb52edb334dacbf25aa27877e738"
      addressBytes = case Base16.decode testAddressHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      addressPattern = defMessage
        & CF.exactAddress .~ addressBytes
      cardanoPattern = defMessage
        & CF.address .~ addressPattern
      anyPattern = defMessage
        & QF.cardano .~ cardanoPattern
      predicate = defMessage
        & QF.match .~ anyPattern
      request = defMessage
        & QF.predicate .~ predicate
        & QF.maxItems .~ 10

  -- Act
  result <- searchUtxos (queryClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)

      -- Show actual data from response
      putStrLn $ "  Found " ++ show (length items) ++ " UTXOs for address"
      when (length items > 0) $ do
        let firstUtxo = head items
        let firstOutput = firstUtxo ^. QF.cardano
        putStrLn $ "  First UTXO coin: " ++ show (firstOutput ^. CF.coin) ++ " lovelace"

      -- Verify all returned UTXOs belong to the searched address
      forM_ items $ \utxo -> do
        let output = utxo ^. QF.cardano
        assertEqual "address matches" addressBytes (output ^. CF.address)

      putStrLn $ "✓ SearchUtxosByAddress test passed! Found " ++ show (length items) ++ " UTXOs"
      
    err -> throwString $ "SearchUtxosByAddress failed: " ++ show err

testSearchUtxosByPaymentPart :: UtxorpcClient -> IO ()
testSearchUtxosByPaymentPart client = do
  putStrLn "\n=== Test: SearchUtxosByPaymentPart ==="

  -- Arrange
  let paymentCredHex = "53fbfffab7b001281917de77f18a8087413be03401db4aa2a7dbf0ae"
      paymentCredBytes = case Base16.decode paymentCredHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      addressPattern = defMessage
        & CF.paymentPart .~ paymentCredBytes
      cardanoPattern = defMessage
        & CF.address .~ addressPattern
      anyPattern = defMessage
        & QF.cardano .~ cardanoPattern
      predicate = defMessage
        & QF.match .~ anyPattern
      request = defMessage
        & QF.predicate .~ predicate
        & QF.maxItems .~ 5

  -- Act
  result <- searchUtxos (queryClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)

      -- Show actual data from response
      putStrLn $ "  Found " ++ show (length items) ++ " UTXOs matching payment credential"

      -- Verify all returned UTXOs have the correct payment credential
      forM_ items $ \utxo -> do
        let output = utxo ^. QF.cardano
        let utxoAddressBytes = output ^. CF.address
        -- Payment credential is bytes 1-28 (after network byte)
        let utxoPaymentCred = BS.take 28 $ BS.drop 1 utxoAddressBytes
        assertEqual "payment credential matches" paymentCredBytes utxoPaymentCred

      putStrLn $ "✓ SearchUtxosByPaymentPart test passed! Found " ++ show (length items) ++ " UTXOs"
      
    err -> throwString $ "SearchUtxosByPaymentPart failed: " ++ show err

testSearchUtxosByDelegationPart :: UtxorpcClient -> IO ()
testSearchUtxosByDelegationPart client = do
  putStrLn "\n=== Test: SearchUtxosByDelegationPart ==="

  -- Arrange
  let delegationCredHex = "1591d34d5b4b2728d04a80fdd041bb52edb334dacbf25aa27877e738"
      delegationCredBytes = case Base16.decode delegationCredHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      addressPattern = defMessage
        & CF.delegationPart .~ delegationCredBytes
      cardanoPattern = defMessage
        & CF.address .~ addressPattern
      anyPattern = defMessage
        & QF.cardano .~ cardanoPattern
      predicate = defMessage
        & QF.match .~ anyPattern
      request = defMessage
        & QF.predicate .~ predicate
        & QF.maxItems .~ 5

  -- Act
  result <- searchUtxos (queryClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)

      -- Show actual data from response
      putStrLn $ "  Found " ++ show (length items) ++ " UTXOs matching delegation credential"

      -- Verify all returned UTXOs have the correct delegation credential
      forM_ items $ \utxo -> do
        let output = utxo ^. QF.cardano
        let utxoAddressBytes = output ^. CF.address
        -- Base address should be 57 bytes: 1 (network) + 28 (payment) + 28 (delegation)
        assertEqual "address length" 57 (BS.length utxoAddressBytes)

        -- Delegation credential is bytes 29-56 (last 28 bytes)
        let utxoDelegationCred = BS.take 28 $ BS.drop 29 utxoAddressBytes
        assertEqual "delegation credential matches" delegationCredBytes utxoDelegationCred

      putStrLn $ "✓ SearchUtxosByDelegationPart test passed! Found " ++ show (length items) ++ " UTXOs"
      
    err -> throwString $ "SearchUtxosByDelegationPart failed: " ++ show err

testSearchUtxosByPolicyID :: UtxorpcClient -> IO ()
testSearchUtxosByPolicyID client = do
  putStrLn "\n=== Test: SearchUtxosByPolicyID ==="

  -- Arrange
  let policyIdHex = "047e0f912c4260fe66ae271e5ae494dcd5f79635bbbb1386be195f4e"
      policyIdBytes = case Base16.decode policyIdHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      assetPattern = defMessage
        & CF.policyId .~ policyIdBytes
      cardanoPattern = defMessage
        & CF.asset .~ assetPattern
      anyPattern = defMessage
        & QF.cardano .~ cardanoPattern
      predicate = defMessage
        & QF.match .~ anyPattern
      request = defMessage
        & QF.predicate .~ predicate
        & QF.maxItems .~ 10

  -- Act
  result <- searchUtxos (queryClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)

      -- Show actual data from response
      putStrLn $ "  Found " ++ show (length items) ++ " UTXOs containing policy ID"
      putStrLn $ "  Policy ID: " ++ T.unpack (T.decodeUtf8 $ Base16.encode policyIdBytes)

      -- Verify all returned UTXOs contain the searched policy ID
      forM_ items $ \utxo -> do
        let output = utxo ^. QF.cardano
        let assetGroups = output ^. CF.assets
        assertTrue "has assets" (length assetGroups > 0)

        -- Check that at least one asset group has the expected policy ID
        let hasExpectedPolicy = any (\ag -> (ag ^. CF.policyId) == policyIdBytes) assetGroups
        assertTrue "contains expected policy ID" hasExpectedPolicy

      putStrLn $ "✓ SearchUtxosByPolicyID test passed! Found " ++ show (length items) ++ " UTXOs"
      
    err -> throwString $ "SearchUtxosByPolicyID failed: " ++ show err

testSearchUtxosByAsset :: UtxorpcClient -> IO ()
testSearchUtxosByAsset client = do
  putStrLn "\n=== Test: SearchUtxosByAsset ==="

  -- Arrange
  let policyIdHex = "047e0f912c4260fe66ae271e5ae494dcd5f79635bbbb1386be195f4e"
      assetNameHex = "414c4c45594b41545a3030303630" -- "ALLEYKATZ00060"
      policyIdBytes = case Base16.decode policyIdHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      assetNameOnlyBytes = case Base16.decode assetNameHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      -- Concatenate policy ID and asset name for the assetName field
      fullAssetNameBytes = policyIdBytes <> assetNameOnlyBytes
      assetPattern = defMessage
        & CF.assetName .~ fullAssetNameBytes
      cardanoPattern = defMessage
        & CF.asset .~ assetPattern
      anyPattern = defMessage
        & QF.cardano .~ cardanoPattern
      predicate = defMessage
        & QF.match .~ anyPattern
      request = defMessage
        & QF.predicate .~ predicate
        & QF.maxItems .~ 10

  -- Act
  result <- searchUtxos (queryClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)

      -- Show actual data from response
      putStrLn $ "  Found " ++ show (length items) ++ " UTXOs containing specific asset"
      putStrLn $ "  Asset: " ++ T.unpack (T.decodeUtf8 assetNameHex) ++ " (ALLEYKATZ00060)"

      -- Verify all returned UTXOs contain the exact asset
      forM_ items $ \utxo -> do
        let output = utxo ^. QF.cardano
        let assetGroups = output ^. CF.assets

        -- Find the asset group with our policy ID
        let maybeAssetGroup = find (\ag -> (ag ^. CF.policyId) == policyIdBytes) assetGroups
        case maybeAssetGroup of
          Just assetGroup -> do
            let assets = assetGroup ^. CF.assets

            -- Check that this group contains our asset name (just the name part, not policy+name)
            let hasExpectedAsset = any (\a -> (a ^. CF.name) == assetNameOnlyBytes) assets
            assertTrue "contains expected asset name" hasExpectedAsset
          Nothing -> throwString "Asset group with expected policy ID not found"

      putStrLn $ "✓ SearchUtxosByAsset test passed! Found " ++ show (length items) ++ " UTXOs"
      
    err -> throwString $ "SearchUtxosByAsset failed: " ++ show err

testSyncFollowTip :: UtxorpcClient -> IO ()
testSyncFollowTip client = do
  putStrLn "\n=== Test: SyncFollowTip ==="
  
  -- Arrange - Use the same test point as the Node SDK
  let testSlot = 85213090
      testHashHex = "e50842b1cc3ac813cb88d1533c3dea0f92e0ea945f53487c1d960c2210d0c3ba"
      testHashBytes = case Base16.decode testHashHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      intersectPoint = defMessage
        & SF.slot .~ testSlot
        & SF.hash .~ testHashBytes
      request = defMessage
        & SF.intersect .~ [intersectPoint]
  
  -- Act - Follow the same pattern as the working example but catch the planned exception
  result <- try $ followTip (syncClient client) (0 :: Int) request streamHandler
  case result of
    Left (exc :: SomeException) -> 
      if "FollowTip test completed successfully" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ SyncFollowTip test passed! Validated block assertions and stopped"
      else
        putStrLn $ "SyncFollowTip failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (messageCount, _headers, _trailers)) -> do
        putStrLn $ "✓ SyncFollowTip test passed! Received " ++ show messageCount ++ " messages"
      err -> putStrLn $ "SyncFollowTip failed: " ++ show err
  where
    streamHandler count _headers (response :: FollowTipResponse) = do
      putStrLn $ "Received stream message #" ++ show (count + 1)
      
      -- Get the block from the response
      let maybeBlock = case response ^. SF.maybe'action of
            Just action -> case action of
              S.FollowTipResponse'Apply applyAction -> Just applyAction
              S.FollowTipResponse'Undo undoAction -> Just undoAction
              _ -> Nothing
            Nothing -> Nothing
      
      case maybeBlock of
        Just block -> do
          -- Assert block has cardano data
          let cardanoBlock = block ^. SF.cardano
          let blockHeader = cardanoBlock ^. CF.header
          let blockSlot = blockHeader ^. SF.slot
          let blockHash = blockHeader ^. SF.hash
          
          -- Assert that slot is a positive number (like other tests)
          assertTrue ("followTip slot > 0 for message #" ++ show (count + 1)) (blockSlot > 0)
          
          -- Assert that hash is not empty (like other tests)
          assertTrue ("followTip hash not empty for message #" ++ show (count + 1)) (BS.length blockHash > 0)
          
          -- Assert that height is reasonable (like other tests)
          let blockHeight = blockHeader ^. SF.height
          assertTrue ("followTip height > 0 for message #" ++ show (count + 1)) (blockHeight > 0)
          
          putStrLn $ "✓ Message #" ++ show (count + 1) ++ " - Slot: " ++ show blockSlot ++ 
                     ", Height: " ++ show blockHeight ++ 
                     ", Hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode blockHash) ++ "..."
          
          -- Stop after validating a few successful messages (like Node SDK tests)
          if count >= 2 then
            throwString "FollowTip test completed successfully - stopping stream"
          else
            return (count + 1)
        Nothing -> do
          putStrLn $ "⚠ Message #" ++ show (count + 1) ++ " - No block data (might be reset action)"
          return (count + 1)

testSyncReadTip :: UtxorpcClient -> IO ()
testSyncReadTip client = do
  putStrLn "\n=== Test: SyncReadTip ==="

  -- Arrange
  let request = defMessage

  -- Act
  result <- readTip (syncClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert - ReadTipResponse has a tip field, not slot/hash directly
      let tip = response ^. SF.tip
      let tipSlot = tip ^. SF.slot
      let tipHash = tip ^. SF.hash

      -- Show actual data from response
      putStrLn $ "  Current tip slot: " ++ show tipSlot
      putStrLn $ "  Current tip hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode tipHash) ++ "..."

      -- Assert that slot is a positive number
      assertTrue "slot > 0" (tipSlot > 0)

      -- Assert that hash is not empty
      assertTrue "hash not empty" (BS.length tipHash > 0)

      putStrLn $ "✓ SyncReadTip test passed!"
      
    err -> throwString $ "SyncReadTip failed: " ++ show err

testSyncFetchBlock :: UtxorpcClient -> IO ()
testSyncFetchBlock client = do
  putStrLn "\n=== Test: SyncFetchBlock ==="

  -- Arrange - Use the same test point as the Node SDK
  let testSlot = 85213090
      testHashHex = "e50842b1cc3ac813cb88d1533c3dea0f92e0ea945f53487c1d960c2210d0c3ba"
      testHashBytes = case Base16.decode testHashHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      blockRef = defMessage
        & SF.slot .~ testSlot
        & SF.hash .~ testHashBytes
      request = defMessage
        & SF.ref .~ [blockRef]

  -- Act
  result <- fetchBlock (syncClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert
      let blocks = response ^. SF.block
      block <- assertSingle "blocks" blocks

      -- Assert block has cardano data
      let cardanoBlock = block ^. SF.cardano
      let blockHeader = cardanoBlock ^. CF.header
      let blockBody = cardanoBlock ^. CF.body
      let txCount = length (blockBody ^. CF.tx)

      -- Show actual data from response
      putStrLn $ "  Block slot: " ++ show (blockHeader ^. SF.slot)
      putStrLn $ "  Block height: " ++ show (blockHeader ^. SF.height)
      putStrLn $ "  Block hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode testHashBytes) ++ "..."
      putStrLn $ "  Transaction count: " ++ show txCount

      assertEqual "block slot" testSlot (blockHeader ^. SF.slot)
      assertEqual "block height" 3399486 (blockHeader ^. SF.height)

      -- Assert block body exists (even if empty)
      assertNotNull "block body" (Just blockBody)

      putStrLn "✓ SyncFetchBlock test passed!"
      
    err -> throwString $ "SyncFetchBlock failed: " ++ show err

testSyncDumpHistory :: UtxorpcClient -> IO ()
testSyncDumpHistory client = do
  putStrLn "\n=== Test: SyncDumpHistory ==="

  -- Arrange - Use the same test point as the Node SDK
  let testSlot = 85213090
      testHashHex = "e50842b1cc3ac813cb88d1533c3dea0f92e0ea945f53487c1d960c2210d0c3ba"
      testHashBytes = case Base16.decode testHashHex of
        Right bs -> bs
        Left err -> error $ "Invalid hex: " ++ err
      startToken = defMessage
        & SF.slot .~ testSlot
        & SF.hash .~ testHashBytes
      request = defMessage
        & SF.startToken .~ startToken
        & SF.maxItems .~ 1

  -- Act
  result <- dumpHistory (syncClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"
      -- Assert
      let blocks = response ^. SF.block
      block <- assertSingle "blocks" blocks

      -- Assert block has cardano data
      let cardanoBlock = block ^. SF.cardano
      let blockHeader = cardanoBlock ^. CF.header
      let blockBody = cardanoBlock ^. CF.body
      let txCount = length (blockBody ^. CF.tx)

      -- Show actual data from response
      putStrLn $ "  Block slot: " ++ show (blockHeader ^. SF.slot)
      putStrLn $ "  Block height: " ++ show (blockHeader ^. SF.height)
      putStrLn $ "  Transaction count: " ++ show txCount

      assertEqual "block slot" testSlot (blockHeader ^. SF.slot)
      assertEqual "block height" 3399486 (blockHeader ^. SF.height)

      -- Assert block body exists (even if empty)
      assertNotNull "block body" (Just blockBody)

      putStrLn "✓ SyncDumpHistory test passed!"

    err -> throwString $ "SyncDumpHistory failed: " ++ show err

-- ============================================================================
-- Submit Tests
-- ============================================================================

testSubmitTx :: UtxorpcClient -> IO ()
testSubmitTx client = do
  putStrLn "\n=== Test: SubmitTx ==="

  -- Arrange - Use a pre-built signed transaction CBOR hex
  let txCborHex = "84A300D9010282825820AAD611BED4B7089272AEE943CF0975544C46626272929EB50BD6A7D319B43BD303825820970727113A6B9D6E54E2541E5FFFD86D6CF7B7CEA4F6793821DE2C0D58B8D6B5030182A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011A005B8D80A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011B00000001D35FFD4C021A00029E8DA100D9010281825820D504D361777F985265A6F0A727E601E70F837574652760456B41731FF4F95258584046563C4A3D87995D5A1EC03632BF1AF532A1468F7F2403C1055676776D03B8F986EC667A2D67329D97052C9216CF9752E38430E1430F0FEE0FFF958E25F6020AF5F6"
      txCborBytes = case Base16.decode (T.encodeUtf8 $ T.pack txCborHex) of
        Right bs -> bs
        Left err -> error $ "Invalid CBOR hex: " ++ err

      -- Create AnyChain with Cardano raw CBOR
      anyChainTx = defMessage
        & SubF.raw .~ txCborBytes

      -- Create request with the transaction
      request = defMessage
        & SubF.tx .~ anyChainTx

  -- Act
  result <- submitTx (submitClient client) request
  case result of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      putStrLn "✓ Response received from server"

      -- Assert.NotNull(response) - implicit (we have response)
      -- Assert.NotNull(response.Refs)
      let txHash = response ^. SubF.ref
      assertNotNull "response.Refs" (if BS.null txHash then Nothing else Just txHash)

      -- Assert.Equal(32, response.Refs.Length)
      assertEqual "Tx hash length" 32 (BS.length txHash)

      putStrLn $ "  Transaction submitted successfully!"
      putStrLn $ "  Tx hash: " ++ T.unpack (T.decodeUtf8 $ Base16.encode txHash)

      putStrLn "✓ SubmitTx test passed!"

    err -> throwString $ "SubmitTx failed: " ++ show err

testWaitForTx :: UtxorpcClient -> IO ()
testWaitForTx client = do
  putStrLn "\n=== Test: WaitForTx ==="

  -- Arrange - First submit a transaction to wait for
  let txCborHex = "84A300D90102828258207478ED8D8B53CDB520E257AADAFD4CA0EC8671ECA1AE0EEE6D045F9ABE357A78018258208841ABB27C76AF2841727ACF562149EBE47957A71C5C12C8A6A1FAE089CCAAD8020182A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011A005B8D80A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011B00000001D3D2B3A2021A00029E8DA100D9010281825820D504D361777F985265A6F0A727E601E70F837574652760456B41731FF4F952585840AC13B5036324F1273A576834F4A11474CC0FF7C79F31B1E1BD79301E671172CC94B7BCA8C816F80BE343A2389998E0E861C53F501614AD5186355B65F228EF0BF5F6"
      txCborBytes = case Base16.decode (T.encodeUtf8 $ T.pack txCborHex) of
        Right bs -> bs
        Left err -> error $ "Invalid CBOR hex: " ++ err

      anyChainTx = defMessage
        & SubF.raw .~ txCborBytes

      submitRequest = defMessage
        & SubF.tx .~ anyChainTx

  -- Submit the transaction first
  submitResult <- submitTx (submitClient client) submitRequest
  txHash <- case submitResult of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      let txHash = response ^. SubF.ref
      assertTrue "has tx ref" (not $ BS.null txHash)
      return txHash
    err -> throwString $ "Failed to submit tx for WaitForTx test: " ++ show err

  putStrLn $ "  Submitted tx hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txHash) ++ "..."

  -- Create WaitForTxRequest with tx hash
  let waitRequest = defMessage
        & SubF.ref .~ [txHash]

  -- Act - Wait for the transaction (collect at least 2 stages)
  result <- try $ waitForTx (submitClient client) [] waitRequest streamHandler
  case result of
    Left (exc :: SomeException) ->
      if "WaitForTx test completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WaitForTx test passed! Validated stages and stopped"
      else
        throwString $ "WaitForTx failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (stages, _headers, _trailers)) -> do
        -- Assert.True(stages.Count >= 1)
        assertTrue "stages.Count >= 1" (length stages >= 1)

        let firstStage = head stages
        -- Assert.NotNull(firstStage) - implicit
        -- Assert.NotNull(firstStage.Ref)
        let stageRef = firstStage ^. SubF.ref
        assertNotNull "firstStage.Ref" (if BS.null stageRef then Nothing else Just stageRef)

        -- Assert.Equal(txHash, firstStage.Ref)
        assertEqual "firstStage.Ref matches txHash" txHash stageRef

        -- Assert.True(firstStage.Stage >= Stage.Acknowledged)
        let stageNum = firstStage ^. SubF.stage
        assertTrue "firstStage.Stage >= Acknowledged" (stageNum >= STAGE_ACKNOWLEDGED)

        putStrLn $ "✓ WaitForTx test passed! Received " ++ show (length stages) ++ " stage updates"
      err -> throwString $ "WaitForTx failed: " ++ show err
  where
    streamHandler :: [WaitForTxResponse] -> a -> WaitForTxResponse -> IO [WaitForTxResponse]
    streamHandler stages _headers response = do
      let stageNum = response ^. SubF.stage
      putStrLn $ "  Stage update #" ++ show (length stages + 1) ++ ": " ++ show stageNum

      let newStages = stages ++ [response]
      -- Stop after 2 stages: if (stages.Count >= 2) break
      if length newStages >= 2 then
        throwString "WaitForTx test completed - stopping stream"
      else
        return newStages

testWatchMempoolByAddress :: UtxorpcClient -> IO ()
testWatchMempoolByAddress client = do
  putStrLn "\n=== Test: WatchMempoolByAddress ==="

  -- Arrange
  let receiverAddressHex = "0053fbfffab7b001281917de77f18a8087413be03401db4aa2a7dbf0ae1591d34d5b4b2728d04a80fdd041bb52edb334dacbf25aa27877e738"
      receiverAddressBytes = case Base16.decode receiverAddressHex of
        Right bs -> bs
        Left err -> error $ "Invalid address hex: " ++ err

      -- Create address predicate for exact address match
      addressPattern = (defMessage :: AddressPattern)
        & CF.exactAddress .~ receiverAddressBytes
      txPattern = (defMessage :: TxPattern)
        & CF.hasAddress .~ addressPattern
      anyChainPattern = (defMessage :: AnyChainTxPattern)
        & SubF.cardano .~ txPattern
      predicate = (defMessage :: TxPredicate)
        & SubF.match .~ anyChainPattern
      watchRequest = defMessage
        & SubF.predicate .~ predicate

  putStrLn $ "  Watching for address: " ++ T.unpack (T.take 20 $ T.decodeUtf8 $ Base16.encode receiverAddressBytes) ++ "..."

  -- Act - Watch mempool (transaction already submitted)
  result <- try $ watchMempool (submitClient client) [] watchRequest streamHandler
  case result of
    Left (exc :: SomeException) ->
      if "WatchMempoolByAddress completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchMempoolByAddress test passed! Found matching transaction"
      else
        throwString $ "WatchMempoolByAddress failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        -- Assert.True(events.Count > 0)
        assertTrue "events.Count > 0" (length events > 0)

        let firstEvent = head events
        -- Assert.NotNull(firstEvent.Tx)
        let maybeTx = firstEvent ^. SubF.maybe'tx
        assertNotNull "firstEvent.Tx" maybeTx

        case maybeTx of
          Just txInfo -> do
            -- Assert.NotNull(firstEvent.Tx?.Ref)
            let txRef = txInfo ^. SubF.ref
            assertNotNull "firstEvent.Tx.Ref" (if BS.null txRef then Nothing else Just txRef)

            -- Assert.True(firstEvent.Tx?.Stage >= Stage.Acknowledged)
            let stage = txInfo ^. SubF.stage
            assertTrue "firstEvent.Tx.Stage >= Acknowledged" (stage >= STAGE_ACKNOWLEDGED)

            putStrLn $ "✓ WatchMempoolByAddress test passed! Found " ++ show (length events) ++ " event(s)"
          Nothing -> throwString "firstEvent.Tx is null"

      err -> throwString $ "WatchMempoolByAddress failed: " ++ show err
  where
    streamHandler :: [WatchMempoolResponse] -> a -> WatchMempoolResponse -> IO [WatchMempoolResponse]
    streamHandler events _headers response = do
      let maybeTx = response ^. SubF.maybe'tx
      case maybeTx of
        Just txInfo -> do
          let txRef = txInfo ^. SubF.ref
          let stage = txInfo ^. SubF.stage
          putStrLn $ "  Found transaction in mempool!"
          putStrLn $ "    Tx ref: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txRef) ++ "..."
          putStrLn $ "    Stage: " ++ show stage

          let newEvents = events ++ [response]
          -- Stop after 1 event: if (events.Count >= 1) break
          if length newEvents >= 1 then
            throwString "WatchMempoolByAddress completed - stopping stream"
          else
            return newEvents
        Nothing -> do
          putStrLn "  Waiting for matching transaction..."
          return events

testWatchMempoolByPaymentPart :: UtxorpcClient -> IO ()
testWatchMempoolByPaymentPart client = do
  putStrLn "\n=== Test: WatchMempoolByPaymentPart ==="

  -- Arrange - Extract payment credential from address
  let paymentCredHex = "53fbfffab7b001281917de77f18a8087413be03401db4aa2a7dbf0ae"
      paymentCredBytes = case Base16.decode paymentCredHex of
        Right bs -> bs
        Left err -> error $ "Invalid payment cred hex: " ++ err

      addressPattern = (defMessage :: AddressPattern)
        & CF.paymentPart .~ paymentCredBytes
      txPattern = (defMessage :: TxPattern)
        & CF.hasAddress .~ addressPattern
      anyChainPattern = (defMessage :: AnyChainTxPattern)
        & SubF.cardano .~ txPattern
      predicate = (defMessage :: TxPredicate)
        & SubF.match .~ anyChainPattern
      watchRequest = defMessage
        & SubF.predicate .~ predicate

  putStrLn $ "  Watching for payment credential: " ++ T.unpack (T.decodeUtf8 $ Base16.encode paymentCredBytes)

  -- Act - Watch mempool
  result <- try $ watchMempool (submitClient client) [] watchRequest streamHandler
  case result of
    Left (exc :: SomeException) ->
      if "WatchMempoolByPaymentPart completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchMempoolByPaymentPart test passed! Found matching transaction"
      else
        throwString $ "WatchMempoolByPaymentPart failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        assertTrue "events.Count > 0" (length events > 0)
        let firstEvent = head events
        let maybeTx = firstEvent ^. SubF.maybe'tx
        assertNotNull "firstEvent.Tx" maybeTx
        case maybeTx of
          Just txInfo -> do
            let txRef = txInfo ^. SubF.ref
            assertNotNull "firstEvent.Tx.Ref" (if BS.null txRef then Nothing else Just txRef)
            let stage = txInfo ^. SubF.stage
            assertTrue "firstEvent.Tx.Stage >= Acknowledged" (stage >= STAGE_ACKNOWLEDGED)
            putStrLn $ "✓ WatchMempoolByPaymentPart test passed!"
          Nothing -> throwString "firstEvent.Tx is null"
      err -> throwString $ "WatchMempoolByPaymentPart failed: " ++ show err
  where
    streamHandler :: [WatchMempoolResponse] -> a -> WatchMempoolResponse -> IO [WatchMempoolResponse]
    streamHandler events _headers response = do
      let maybeTx = response ^. SubF.maybe'tx
      case maybeTx of
        Just txInfo -> do
          let txRef = txInfo ^. SubF.ref
          let stage = txInfo ^. SubF.stage
          putStrLn $ "  Found transaction matching payment credential!"
          putStrLn $ "    Tx ref: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txRef) ++ "..."
          putStrLn $ "    Stage: " ++ show stage
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchMempoolByPaymentPart completed - stopping stream"
          else
            return newEvents
        Nothing -> return events

testWatchMempoolByDelegationPart :: UtxorpcClient -> IO ()
testWatchMempoolByDelegationPart client = do
  putStrLn "\n=== Test: WatchMempoolByDelegationPart ==="

  -- Arrange - Extract delegation credential from address
  let delegationCredHex = "1591d34d5b4b2728d04a80fdd041bb52edb334dacbf25aa27877e738"
      delegationCredBytes = case Base16.decode delegationCredHex of
        Right bs -> bs
        Left err -> error $ "Invalid delegation cred hex: " ++ err

      addressPattern = (defMessage :: AddressPattern)
        & CF.delegationPart .~ delegationCredBytes
      txPattern = (defMessage :: TxPattern)
        & CF.hasAddress .~ addressPattern
      anyChainPattern = (defMessage :: AnyChainTxPattern)
        & SubF.cardano .~ txPattern
      predicate = (defMessage :: TxPredicate)
        & SubF.match .~ anyChainPattern
      watchRequest = defMessage
        & SubF.predicate .~ predicate

  putStrLn $ "  Watching for delegation credential: " ++ T.unpack (T.decodeUtf8 $ Base16.encode delegationCredBytes)

  -- Act - Watch mempool
  result <- try $ watchMempool (submitClient client) [] watchRequest streamHandler
  case result of
    Left (exc :: SomeException) ->
      if "WatchMempoolByDelegationPart completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchMempoolByDelegationPart test passed! Found matching transaction"
      else
        throwString $ "WatchMempoolByDelegationPart failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        assertTrue "events.Count > 0" (length events > 0)
        let firstEvent = head events
        let maybeTx = firstEvent ^. SubF.maybe'tx
        assertNotNull "firstEvent.Tx" maybeTx
        case maybeTx of
          Just txInfo -> do
            let txRef = txInfo ^. SubF.ref
            assertNotNull "firstEvent.Tx.Ref" (if BS.null txRef then Nothing else Just txRef)
            let stage = txInfo ^. SubF.stage
            assertTrue "firstEvent.Tx.Stage >= Acknowledged" (stage >= STAGE_ACKNOWLEDGED)
            putStrLn $ "✓ WatchMempoolByDelegationPart test passed!"
          Nothing -> throwString "firstEvent.Tx is null"
      err -> throwString $ "WatchMempoolByDelegationPart failed: " ++ show err
  where
    streamHandler :: [WatchMempoolResponse] -> a -> WatchMempoolResponse -> IO [WatchMempoolResponse]
    streamHandler events _headers response = do
      let maybeTx = response ^. SubF.maybe'tx
      case maybeTx of
        Just txInfo -> do
          let txRef = txInfo ^. SubF.ref
          let stage = txInfo ^. SubF.stage
          putStrLn $ "  Found transaction matching delegation credential!"
          putStrLn $ "    Tx ref: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txRef) ++ "..."
          putStrLn $ "    Stage: " ++ show stage
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchMempoolByDelegationPart completed - stopping stream"
          else
            return newEvents
        Nothing -> return events

testWatchMempoolByPolicyId :: UtxorpcClient -> IO ()
testWatchMempoolByPolicyId client = do
  putStrLn "\n=== Test: WatchMempoolByPolicyId ==="

  -- Arrange
  let policyIdHex = "8b05e87a51c1d4a0fa888d2bb14dbc25e8c343ea379a171b63aa84a0"
      policyIdBytes = case Base16.decode policyIdHex of
        Right bs -> bs
        Left err -> error $ "Invalid policy ID hex: " ++ err

      assetPattern = (defMessage :: AssetPattern)
        & CF.policyId .~ policyIdBytes
      txPattern = (defMessage :: TxPattern)
        & CF.movesAsset .~ assetPattern
      anyChainPattern = (defMessage :: AnyChainTxPattern)
        & SubF.cardano .~ txPattern
      predicate = (defMessage :: TxPredicate)
        & SubF.match .~ anyChainPattern
      watchRequest = defMessage
        & SubF.predicate .~ predicate

  putStrLn $ "  Watching for policy ID: " ++ T.unpack (T.decodeUtf8 $ Base16.encode policyIdBytes)

  -- Act - Watch mempool
  result <- try $ watchMempool (submitClient client) [] watchRequest streamHandler
  case result of
    Left (exc :: SomeException) ->
      if "WatchMempoolByPolicyId completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchMempoolByPolicyId test passed! Found matching transaction"
      else
        throwString $ "WatchMempoolByPolicyId failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        assertTrue "events.Count > 0" (length events > 0)
        let firstEvent = head events
        let maybeTx = firstEvent ^. SubF.maybe'tx
        assertNotNull "firstEvent.Tx" maybeTx
        case maybeTx of
          Just txInfo -> do
            let txRef = txInfo ^. SubF.ref
            assertNotNull "firstEvent.Tx.Ref" (if BS.null txRef then Nothing else Just txRef)
            let stage = txInfo ^. SubF.stage
            assertTrue "firstEvent.Tx.Stage >= Acknowledged" (stage >= STAGE_ACKNOWLEDGED)
            putStrLn $ "✓ WatchMempoolByPolicyId test passed!"
          Nothing -> throwString "firstEvent.Tx is null"
      err -> throwString $ "WatchMempoolByPolicyId failed: " ++ show err
  where
    streamHandler :: [WatchMempoolResponse] -> a -> WatchMempoolResponse -> IO [WatchMempoolResponse]
    streamHandler events _headers response = do
      let maybeTx = response ^. SubF.maybe'tx
      case maybeTx of
        Just txInfo -> do
          let txRef = txInfo ^. SubF.ref
          let stage = txInfo ^. SubF.stage
          putStrLn $ "  Found transaction with policy ID!"
          putStrLn $ "    Tx ref: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txRef) ++ "..."
          putStrLn $ "    Stage: " ++ show stage
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchMempoolByPolicyId completed - stopping stream"
          else
            return newEvents
        Nothing -> return events

testWatchMempoolByAsset :: UtxorpcClient -> IO ()
testWatchMempoolByAsset client = do
  putStrLn "\n=== Test: WatchMempoolByAsset ==="

  -- Arrange
  let policyIdHex = "8b05e87a51c1d4a0fa888d2bb14dbc25e8c343ea379a171b63aa84a0"
      assetNameHex = "434e4354" -- "CNCT" in hex
      policyIdBytes = case Base16.decode policyIdHex of
        Right bs -> bs
        Left err -> error $ "Invalid policy ID hex: " ++ err
      assetNameOnlyBytes = case Base16.decode assetNameHex of
        Right bs -> bs
        Left err -> error $ "Invalid asset name hex: " ++ err
      -- Concatenate policy ID and asset name
      fullAssetNameBytes = policyIdBytes <> assetNameOnlyBytes

      assetPattern = (defMessage :: AssetPattern)
        & CF.policyId .~ policyIdBytes
        & CF.assetName .~ assetNameOnlyBytes
      txPattern = (defMessage :: TxPattern)
        & CF.movesAsset .~ assetPattern
      anyChainPattern = (defMessage :: AnyChainTxPattern)
        & SubF.cardano .~ txPattern
      predicate = (defMessage :: TxPredicate)
        & SubF.match .~ anyChainPattern
      watchRequest = defMessage
        & SubF.predicate .~ predicate

  putStrLn $ "  Watching for asset: " ++ T.unpack (T.decodeUtf8 assetNameHex) ++ " (CNCT)"

  -- Act - Watch mempool
  result <- try $ watchMempool (submitClient client) [] watchRequest streamHandler
  case result of
    Left (exc :: SomeException) ->
      if "WatchMempoolByAsset completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchMempoolByAsset test passed! Found matching transaction"
      else
        throwString $ "WatchMempoolByAsset failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        assertTrue "events.Count > 0" (length events > 0)
        let firstEvent = head events
        let maybeTx = firstEvent ^. SubF.maybe'tx
        assertNotNull "firstEvent.Tx" maybeTx
        case maybeTx of
          Just txInfo -> do
            let txRef = txInfo ^. SubF.ref
            assertNotNull "firstEvent.Tx.Ref" (if BS.null txRef then Nothing else Just txRef)
            let stage = txInfo ^. SubF.stage
            assertTrue "firstEvent.Tx.Stage >= Acknowledged" (stage >= STAGE_ACKNOWLEDGED)
            putStrLn $ "✓ WatchMempoolByAsset test passed!"
          Nothing -> throwString "firstEvent.Tx is null"
      err -> throwString $ "WatchMempoolByAsset failed: " ++ show err
  where
    streamHandler :: [WatchMempoolResponse] -> a -> WatchMempoolResponse -> IO [WatchMempoolResponse]
    streamHandler events _headers response = do
      let maybeTx = response ^. SubF.maybe'tx
      case maybeTx of
        Just txInfo -> do
          let txRef = txInfo ^. SubF.ref
          let stage = txInfo ^. SubF.stage
          putStrLn $ "  Found transaction with specific asset!"
          putStrLn $ "    Tx ref: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txRef) ++ "..."
          putStrLn $ "    Stage: " ++ show stage
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchMempoolByAsset completed - stopping stream"
          else
            return newEvents
        Nothing -> return events

-- ============================================================================
-- Watch Tests
-- ============================================================================

testWatchTxForAddress :: UtxorpcClient -> IO ()
testWatchTxForAddress client = do
  putStrLn "\n=== Test: WatchTxForAddress ==="

  -- Arrange
  let receiverAddressHex = "0053fbfffab7b001281917de77f18a8087413be03401db4aa2a7dbf0ae1591d34d5b4b2728d04a80fdd041bb52edb334dacbf25aa27877e738"
      receiverAddressBytes = case Base16.decode receiverAddressHex of
        Right bs -> bs
        Left err -> error $ "Invalid address hex: " ++ err

      -- Create address predicate for exact address match
      addressPattern = (defMessage :: AddressPattern)
        & CF.exactAddress .~ receiverAddressBytes
      txPattern = (defMessage :: TxPattern)
        & CF.hasAddress .~ addressPattern
      anyChainPattern = (defMessage :: W.AnyChainTxPattern)
        & WF.cardano .~ txPattern
      predicate = (defMessage :: W.TxPredicate)
        & WF.match .~ anyChainPattern
      watchRequest = defMessage
        & WF.predicate .~ predicate

  putStrLn $ "  Watching for address: " ++ T.unpack (T.take 20 $ T.decodeUtf8 $ Base16.encode receiverAddressBytes) ++ "..."

  -- Create MVar to store watcher result
  resultVar <- newEmptyMVar

  -- Start watching in background (before submitting transaction)
  _ <- forkIO $ do
    result <- try $ watchTx (watchClient client) [] watchRequest streamHandler
    putMVar resultVar result

  -- Give the watcher time to start
  putStrLn "  Started watching, waiting 1 second for watcher to initialize..."
  threadDelay 1000000  -- 1 second in microseconds

  -- Submit transaction
  putStrLn "  Submitting transaction..."
  let txCborHex = "84A300D90102828258209E0CC477DF20C37C684E47B9701F995C8733B07901DE5447FE4F61EDF14F6BCB01825820176A54A6E3911FF5C65134928079E40783C8A88F1143091687F3FE7775EEF55F000182A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011A005B8D80A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011B00000001D4478888021A00029E8DA100D9010281825820D504D361777F985265A6F0A727E601E70F837574652760456B41731FF4F952585840DBBF46C2B615D9EE1689F6339A537A3A16507C2566E067A717F0D379D9944F8FEF1045AA525B71EB6E29877BFAEA83F295F25707B0F7AF41222E3F33C9861C0CF5F6"
      txCborBytes = case Base16.decode (T.encodeUtf8 $ T.pack txCborHex) of
        Right bs -> bs
        Left err -> error $ "Invalid CBOR hex: " ++ err
      anyChainTx = defMessage & SubF.raw .~ txCborBytes
      submitRequest = defMessage & SubF.tx .~ anyChainTx

  submitResult <- submitTx (submitClient client) submitRequest
  case submitResult of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      let txHash = response ^. SubF.ref
      when (not (BS.null txHash)) $ do
        putStrLn $ "  Transaction submitted! Hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txHash) ++ "..."
    _ -> putStrLn "  Warning: Transaction submission may have issues"

  putStrLn "  Waiting for Watch event (transaction to be applied to block)..."

  -- Wait for watcher result
  result <- takeMVar resultVar

  -- Assert
  case result of
    Left (exc :: SomeException) ->
      if "WatchTxForAddress completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchTxForAddress test passed! Found matching transaction in block"
      else
        throwString $ "WatchTxForAddress failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        -- Assert.True(events.Count > 0)
        assertTrue "events.Count > 0" (length events > 0)

        let firstEvent = head events
        -- Assert.NotNull(firstEvent)
        let maybeAction = firstEvent ^. WF.maybe'action
        assertNotNull "firstEvent.Action" maybeAction

        -- Assert.Equal(WatchTxAction.Apply, firstEvent.Action)
        case maybeAction of
          Just (WatchTxResponse'Apply _) ->
            putStrLn $ "✓ WatchTxForAddress test passed! Found " ++ show (length events) ++ " Apply event(s)"
          _ -> throwString "Expected Apply action"

      err -> throwString $ "WatchTxForAddress failed: " ++ show err
  where
    streamHandler :: [WatchTxResponse] -> a -> WatchTxResponse -> IO [WatchTxResponse]
    streamHandler events _headers response = do
      let maybeAction = response ^. WF.maybe'action
      case maybeAction of
        Just (WatchTxResponse'Apply _) -> do
          putStrLn $ "  Found Apply action for transaction!"
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchTxForAddress completed - stopping stream"
          else
            return newEvents
        _ -> return events

testWatchTxForPaymentPart :: UtxorpcClient -> IO ()
testWatchTxForPaymentPart client = do
  putStrLn "\n=== Test: WatchTxForPaymentPart ==="

  -- Arrange - Extract payment credential from address
  let paymentCredHex = "53fbfffab7b001281917de77f18a8087413be03401db4aa2a7dbf0ae"
      paymentCredBytes = case Base16.decode paymentCredHex of
        Right bs -> bs
        Left err -> error $ "Invalid payment cred hex: " ++ err

      addressPattern = (defMessage :: AddressPattern)
        & CF.paymentPart .~ paymentCredBytes
      txPattern = (defMessage :: TxPattern)
        & CF.hasAddress .~ addressPattern
      anyChainPattern = (defMessage :: W.AnyChainTxPattern)
        & WF.cardano .~ txPattern
      predicate = (defMessage :: W.TxPredicate)
        & WF.match .~ anyChainPattern
      watchRequest = defMessage
        & WF.predicate .~ predicate

  putStrLn $ "  Watching for payment credential: " ++ T.unpack (T.decodeUtf8 $ Base16.encode paymentCredBytes)

  -- Create MVar to store watcher result
  resultVar <- newEmptyMVar

  -- Start watching in background
  _ <- forkIO $ do
    result <- try $ watchTx (watchClient client) [] watchRequest streamHandler
    putMVar resultVar result

  -- Give the watcher time to start
  putStrLn "  Started watching, waiting 1 second..."
  threadDelay 1000000

  -- Submit transaction
  putStrLn "  Submitting transaction..."
  let txCborHex = "84A300D9010282825820896E271D8CC28CB59E9E9D0464C06D3C2A48A0766438AD9FFBABC9597C4E072101825820CE5996492EA64AE09B9D06A062FD8C98D2231A325FF48071C811729E8B7D420E000182A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011A005B8D80A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011B00000001D481F2FB021A00029E8DA100D9010281825820D504D361777F985265A6F0A727E601E70F837574652760456B41731FF4F952585840A92445E02162D6F7212713A41124B8ED260DB632EC37A9F77BEB1EE813E108813F216A06CEBCB42708D1C87CD20F861F395B59871FE9A0CB9CC6A294C6FE4104F5F6"
      txCborBytes = case Base16.decode (T.encodeUtf8 $ T.pack txCborHex) of
        Right bs -> bs
        Left err -> error $ "Invalid CBOR hex: " ++ err
      anyChainTx = defMessage & SubF.raw .~ txCborBytes
      submitRequest = defMessage & SubF.tx .~ anyChainTx

  submitResult <- submitTx (submitClient client) submitRequest
  case submitResult of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      let txHash = response ^. SubF.ref
      when (not (BS.null txHash)) $ do
        putStrLn $ "  Transaction submitted! Hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txHash) ++ "..."
    _ -> putStrLn "  Warning: Transaction submission may have issues"

  putStrLn "  Waiting for Watch event..."

  -- Wait for watcher result
  result <- takeMVar resultVar

  -- Assert
  case result of
    Left (exc :: SomeException) ->
      if "WatchTxForPaymentPart completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchTxForPaymentPart test passed! Found matching transaction"
      else
        throwString $ "WatchTxForPaymentPart failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        assertTrue "events.Count > 0" (length events > 0)
        let firstEvent = head events
        let maybeAction = firstEvent ^. WF.maybe'action
        assertNotNull "firstEvent.Action" maybeAction
        case maybeAction of
          Just (WatchTxResponse'Apply _) ->
            putStrLn $ "✓ WatchTxForPaymentPart test passed!"
          _ -> throwString "Expected Apply action"
      err -> throwString $ "WatchTxForPaymentPart failed: " ++ show err
  where
    streamHandler :: [WatchTxResponse] -> a -> WatchTxResponse -> IO [WatchTxResponse]
    streamHandler events _headers response = do
      let maybeAction = response ^. WF.maybe'action
      case maybeAction of
        Just (WatchTxResponse'Apply _) -> do
          putStrLn $ "  Found Apply action for transaction matching payment credential!"
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchTxForPaymentPart completed - stopping stream"
          else
            return newEvents
        _ -> return events

testWatchTxForDelegationPart :: UtxorpcClient -> IO ()
testWatchTxForDelegationPart client = do
  putStrLn "\n=== Test: WatchTxForDelegationPart ==="

  -- Arrange - Extract delegation credential from address
  let delegationCredHex = "1591d34d5b4b2728d04a80fdd041bb52edb334dacbf25aa27877e738"
      delegationCredBytes = case Base16.decode delegationCredHex of
        Right bs -> bs
        Left err -> error $ "Invalid delegation cred hex: " ++ err

      addressPattern = (defMessage :: AddressPattern)
        & CF.delegationPart .~ delegationCredBytes
      txPattern = (defMessage :: TxPattern)
        & CF.hasAddress .~ addressPattern
      anyChainPattern = (defMessage :: W.AnyChainTxPattern)
        & WF.cardano .~ txPattern
      predicate = (defMessage :: W.TxPredicate)
        & WF.match .~ anyChainPattern
      watchRequest = defMessage
        & WF.predicate .~ predicate

  putStrLn $ "  Watching for delegation credential: " ++ T.unpack (T.decodeUtf8 $ Base16.encode delegationCredBytes)

  -- Create MVar to store watcher result
  resultVar <- newEmptyMVar

  -- Start watching in background
  _ <- forkIO $ do
    result <- try $ watchTx (watchClient client) [] watchRequest streamHandler
    putMVar resultVar result

  -- Give the watcher time to start
  putStrLn "  Started watching, waiting 1 second..."
  threadDelay 1000000

  -- Submit transaction
  putStrLn "  Submitting transaction..."
  let txCborHex = "84A300D90102828258201F0716BFC556282969C9C3CC41E8F598A4D668AAE0120554BBFBE1D6DA2AD1CC01825820488B24DAE71016C45A8BEE133185D0FA22DC18F19C24C464898810B1944D5A28000182A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011A005B8D80A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011B00000001D4BC5D6E021A00029E8DA100D9010281825820D504D361777F985265A6F0A727E601E70F837574652760456B41731FF4F95258584054224628C469C35313471E6C274C93218D0B40093307E9B70A0A246F697D0870CB81E9333FAB6FE7B297ECC0ADA9E9145F0B46AA3E2395A3DECF42970D1B510DF5F6"
      txCborBytes = case Base16.decode (T.encodeUtf8 $ T.pack txCborHex) of
        Right bs -> bs
        Left err -> error $ "Invalid CBOR hex: " ++ err
      anyChainTx = defMessage & SubF.raw .~ txCborBytes
      submitRequest = defMessage & SubF.tx .~ anyChainTx

  submitResult <- submitTx (submitClient client) submitRequest
  case submitResult of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      let txHash = response ^. SubF.ref
      when (not (BS.null txHash)) $ do
        putStrLn $ "  Transaction submitted! Hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txHash) ++ "..."
    _ -> putStrLn "  Warning: Transaction submission may have issues"

  putStrLn "  Waiting for Watch event..."

  -- Wait for watcher result
  result <- takeMVar resultVar

  -- Assert
  case result of
    Left (exc :: SomeException) ->
      if "WatchTxForDelegationPart completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchTxForDelegationPart test passed! Found matching transaction"
      else
        throwString $ "WatchTxForDelegationPart failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        assertTrue "events.Count > 0" (length events > 0)
        let firstEvent = head events
        let maybeAction = firstEvent ^. WF.maybe'action
        assertNotNull "firstEvent.Action" maybeAction
        case maybeAction of
          Just (WatchTxResponse'Apply _) ->
            putStrLn $ "✓ WatchTxForDelegationPart test passed!"
          _ -> throwString "Expected Apply action"
      err -> throwString $ "WatchTxForDelegationPart failed: " ++ show err
  where
    streamHandler :: [WatchTxResponse] -> a -> WatchTxResponse -> IO [WatchTxResponse]
    streamHandler events _headers response = do
      let maybeAction = response ^. WF.maybe'action
      case maybeAction of
        Just (WatchTxResponse'Apply _) -> do
          putStrLn $ "  Found Apply action for transaction matching delegation credential!"
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchTxForDelegationPart completed - stopping stream"
          else
            return newEvents
        _ -> return events

testWatchTxForPolicyId :: UtxorpcClient -> IO ()
testWatchTxForPolicyId client = do
  putStrLn "\n=== Test: WatchTxForPolicyId ==="

  -- Arrange
  let policyIdHex = "8b05e87a51c1d4a0fa888d2bb14dbc25e8c343ea379a171b63aa84a0"
      policyIdBytes = case Base16.decode policyIdHex of
        Right bs -> bs
        Left err -> error $ "Invalid policy ID hex: " ++ err

      assetPattern = (defMessage :: AssetPattern)
        & CF.policyId .~ policyIdBytes
      txPattern = (defMessage :: TxPattern)
        & CF.movesAsset .~ assetPattern
      anyChainPattern = (defMessage :: W.AnyChainTxPattern)
        & WF.cardano .~ txPattern
      predicate = (defMessage :: W.TxPredicate)
        & WF.match .~ anyChainPattern
      watchRequest = defMessage
        & WF.predicate .~ predicate

  putStrLn $ "  Watching for policy ID: " ++ T.unpack (T.decodeUtf8 $ Base16.encode policyIdBytes)

  -- Create MVar to store watcher result
  resultVar <- newEmptyMVar

  -- Start watching in background
  _ <- forkIO $ do
    result <- try $ watchTx (watchClient client) [] watchRequest streamHandler
    putMVar resultVar result

  -- Give the watcher time to start
  putStrLn "  Started watching, waiting 1 second..."
  threadDelay 1000000

  -- Submit transaction
  putStrLn "  Submitting transaction..."
  let txCborHex = "84A300D90102828258209E0CC477DF20C37C684E47B9701F995C8733B07901DE5447FE4F61EDF14F6BCB01825820176A54A6E3911FF5C65134928079E40783C8A88F1143091687F3FE7775EEF55F000182A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011A005B8D80A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011B00000001D4478888021A00029E8DA100D9010281825820D504D361777F985265A6F0A727E601E70F837574652760456B41731FF4F952585840DBBF46C2B615D9EE1689F6339A537A3A16507C2566E067A717F0D379D9944F8FEF1045AA525B71EB6E29877BFAEA83F295F25707B0F7AF41222E3F33C9861C0CF5F6"
      txCborBytes = case Base16.decode (T.encodeUtf8 $ T.pack txCborHex) of
        Right bs -> bs
        Left err -> error $ "Invalid CBOR hex: " ++ err
      anyChainTx = defMessage & SubF.raw .~ txCborBytes
      submitRequest = defMessage & SubF.tx .~ anyChainTx

  submitResult <- submitTx (submitClient client) submitRequest
  case submitResult of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      let txHash = response ^. SubF.ref
      when (not (BS.null txHash)) $ do
        putStrLn $ "  Transaction submitted! Hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txHash) ++ "..."
    _ -> putStrLn "  Warning: Transaction submission may have issues"

  putStrLn "  Waiting for Watch event..."

  -- Wait for watcher result
  result <- takeMVar resultVar

  -- Assert
  case result of
    Left (exc :: SomeException) ->
      if "WatchTxForPolicyId completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchTxForPolicyId test passed! Found matching transaction"
      else
        throwString $ "WatchTxForPolicyId failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        assertTrue "events.Count > 0" (length events > 0)
        let firstEvent = head events
        let maybeAction = firstEvent ^. WF.maybe'action
        assertNotNull "firstEvent.Action" maybeAction
        case maybeAction of
          Just (WatchTxResponse'Apply _) ->
            putStrLn $ "✓ WatchTxForPolicyId test passed!"
          _ -> throwString "Expected Apply action"
      err -> throwString $ "WatchTxForPolicyId failed: " ++ show err
  where
    streamHandler :: [WatchTxResponse] -> a -> WatchTxResponse -> IO [WatchTxResponse]
    streamHandler events _headers response = do
      let maybeAction = response ^. WF.maybe'action
      case maybeAction of
        Just (WatchTxResponse'Apply _) -> do
          putStrLn $ "  Found Apply action for transaction with policy ID!"
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchTxForPolicyId completed - stopping stream"
          else
            return newEvents
        _ -> return events

testWatchTxForAsset :: UtxorpcClient -> IO ()
testWatchTxForAsset client = do
  putStrLn "\n=== Test: WatchTxForAsset ==="

  -- Arrange
  let policyIdHex = "8b05e87a51c1d4a0fa888d2bb14dbc25e8c343ea379a171b63aa84a0"
      assetNameHex = "434e4354" -- "CNCT" in hex
      policyIdBytes = case Base16.decode policyIdHex of
        Right bs -> bs
        Left err -> error $ "Invalid policy ID hex: " ++ err
      assetNameOnlyBytes = case Base16.decode assetNameHex of
        Right bs -> bs
        Left err -> error $ "Invalid asset name hex: " ++ err

      assetPattern = (defMessage :: AssetPattern)
        & CF.policyId .~ policyIdBytes
        & CF.assetName .~ assetNameOnlyBytes
      txPattern = (defMessage :: TxPattern)
        & CF.movesAsset .~ assetPattern
      anyChainPattern = (defMessage :: W.AnyChainTxPattern)
        & WF.cardano .~ txPattern
      predicate = (defMessage :: W.TxPredicate)
        & WF.match .~ anyChainPattern
      watchRequest = defMessage
        & WF.predicate .~ predicate

  putStrLn $ "  Watching for asset: " ++ T.unpack (T.decodeUtf8 assetNameHex) ++ " (CNCT)"

  -- Create MVar to store watcher result
  resultVar <- newEmptyMVar

  -- Start watching in background
  _ <- forkIO $ do
    result <- try $ watchTx (watchClient client) [] watchRequest streamHandler
    putMVar resultVar result

  -- Give the watcher time to start
  putStrLn "  Started watching, waiting 1 second..."
  threadDelay 1000000

  -- Submit transaction
  putStrLn "  Submitting transaction..."
  let txCborHex = "84A300D90102828258209E0CC477DF20C37C684E47B9701F995C8733B07901DE5447FE4F61EDF14F6BCB01825820176A54A6E3911FF5C65134928079E40783C8A88F1143091687F3FE7775EEF55F000182A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011A005B8D80A20058390053FBFFFAB7B001281917DE77F18A8087413BE03401DB4AA2A7DBF0AE1591D34D5B4B2728D04A80FDD041BB52EDB334DACBF25AA27877E738011B00000001D4478888021A00029E8DA100D9010281825820D504D361777F985265A6F0A727E601E70F837574652760456B41731FF4F952585840DBBF46C2B615D9EE1689F6339A537A3A16507C2566E067A717F0D379D9944F8FEF1045AA525B71EB6E29877BFAEA83F295F25707B0F7AF41222E3F33C9861C0CF5F6"
      txCborBytes = case Base16.decode (T.encodeUtf8 $ T.pack txCborHex) of
        Right bs -> bs
        Left err -> error $ "Invalid CBOR hex: " ++ err
      anyChainTx = defMessage & SubF.raw .~ txCborBytes
      submitRequest = defMessage & SubF.tx .~ anyChainTx

  submitResult <- submitTx (submitClient client) submitRequest
  case submitResult of
    Right (Right (Right (_headers, _trailers, Right response))) -> do
      let txHash = response ^. SubF.ref
      when (not (BS.null txHash)) $ do
        putStrLn $ "  Transaction submitted! Hash: " ++ T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode txHash) ++ "..."
    _ -> putStrLn "  Warning: Transaction submission may have issues"

  putStrLn "  Waiting for Watch event..."

  -- Wait for watcher result
  result <- takeMVar resultVar

  -- Assert
  case result of
    Left (exc :: SomeException) ->
      if "WatchTxForAsset completed" `T.isInfixOf` T.pack (show exc) then
        putStrLn "✓ WatchTxForAsset test passed! Found matching transaction"
      else
        throwString $ "WatchTxForAsset failed: " ++ show exc
    Right streamResult -> case streamResult of
      Right (Right (events, _headers, _trailers)) -> do
        assertTrue "events.Count > 0" (length events > 0)
        let firstEvent = head events
        let maybeAction = firstEvent ^. WF.maybe'action
        assertNotNull "firstEvent.Action" maybeAction
        case maybeAction of
          Just (WatchTxResponse'Apply _) ->
            putStrLn $ "✓ WatchTxForAsset test passed!"
          _ -> throwString "Expected Apply action"
      err -> throwString $ "WatchTxForAsset failed: " ++ show err
  where
    streamHandler :: [WatchTxResponse] -> a -> WatchTxResponse -> IO [WatchTxResponse]
    streamHandler events _headers response = do
      let maybeAction = response ^. WF.maybe'action
      case maybeAction of
        Just (WatchTxResponse'Apply _) -> do
          putStrLn $ "  Found Apply action for transaction with specific asset!"
          let newEvents = events ++ [response]
          if length newEvents >= 1 then
            throwString "WatchTxForAsset completed - stopping stream"
          else
            return newEvents
        _ -> return events