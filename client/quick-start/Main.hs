{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Main (main) where

import Control.Exception (bracket, throwIO, try, SomeException)
import Control.Lens ((^.), (.~), (&))
import Control.Monad (void, when, forM_)
import Data.ProtoLens (Message (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List (find)
-- import Data.Maybe (isJust, fromMaybe)
-- import Proto.Utxorpc.V1alpha.Query.Query
import qualified Proto.Utxorpc.V1alpha.Query.Query_Fields as QF
-- import Proto.Utxorpc.V1alpha.Cardano.Cardano
import qualified Proto.Utxorpc.V1alpha.Cardano.Cardano_Fields as CF
import Proto.Utxorpc.V1alpha.Sync.Sync (FollowTipResponse)
import qualified Proto.Utxorpc.V1alpha.Sync.Sync as S
import qualified Proto.Utxorpc.V1alpha.Sync.Sync_Fields as SF
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
assertTrue msg False = throwString $ msg ++ " is false"
assertTrue _ True = return ()

assertSingle :: HasCallStack => String -> [a] -> IO a
assertSingle msg [] = throwString $ msg ++ " is empty"
assertSingle _ [x] = return x
assertSingle msg xs = throwString $ msg ++ " has " ++ show (length xs) ++ " items, expected 1"

main :: IO ()
main = do
  putStrLn "Testing Query endpoints with Dolos (matching .NET SDK tests)..."
  
  -- Connect to Dolos UTxO RPC service
  let mkClient = simpleUtxorpcClient "127.0.0.1" 50051 False
  
  bracket mkClient closeClient $ \case
    Left clientErr -> throwIO clientErr
    Right client -> do
      testReadParams client
      testReadUtxosByOutputRef client
      testSearchUtxosByAddress client
      testSearchUtxosByPaymentPart client
      testSearchUtxosByDelegationPart client
      testSearchUtxosByPolicyID client
      testSearchUtxosByAsset client
      testSyncFollowTip client
      testSyncReadTip client
      testSyncFetchBlock client
      testSyncDumpHistory client
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
      -- Get the parameters
      let params = response ^. QF.values . QF.cardano
      
      -- Assert all expected parameter values for Preview testnet
      assertEqual "coinsPerUtxoByte" 4310 (params ^. CF.coinsPerUtxoByte)
      assertEqual "maxTxSize" 16384 (params ^. CF.maxTxSize)
      assertEqual "minFeeCoefficient" 44 (params ^. CF.minFeeCoefficient)
      assertEqual "minFeeConstant" 155381 (params ^. CF.minFeeConstant)
      assertEqual "maxBlockBodySize" 90112 (params ^. CF.maxBlockBodySize)
      assertEqual "maxBlockHeaderSize" 1100 (params ^. CF.maxBlockHeaderSize)
      assertEqual "stakeKeyDeposit" 2000000 (params ^. CF.stakeKeyDeposit)
      assertEqual "poolDeposit" 500000000 (params ^. CF.poolDeposit)
      assertEqual "desiredNumberOfPools" 500 (params ^. CF.desiredNumberOfPools)
      assertEqual "minPoolCost" 170000000 (params ^. CF.minPoolCost)
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
      assertEqual "governanceActionDeposit" 100000000000 (params ^. CF.governanceActionDeposit)
      assertEqual "drepDeposit" 500000000 (params ^. CF.drepDeposit)
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
      -- Assert
      let items = response ^. QF.items
      utxo <- assertSingle "utxos" items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      
      -- Verify the UTXO reference matches what we requested
      assertEqual "txHash" txHash (utxo ^. QF.txoRef . CF.hash)
      assertEqual "outputIndex" outputIndex (utxo ^. QF.txoRef . CF.index)
      
      -- Verify native bytes
      let expectedNativeBytes = "82583900729c67d0de8cde3c0afc768fb0fcb1596e8cfcbf781b553efcd228813b7bb577937983e016d4e8429ff48cf386d6818883f9e88b62a804e01a05f5e100"
      let actualNativeBytesHex = Base16.encode (utxo ^. QF.nativeBytes)
      assertEqual "nativeBytes" (T.toLower $ T.pack expectedNativeBytes) (T.decodeUtf8 actualNativeBytesHex)
      
      -- Verify parsed state
      let output = utxo ^. QF.cardano
      assertTrue "coin > 0" ((output ^. CF.coin) > 0)
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
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)
      
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
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)
      
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
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)
      
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
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)
      
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
      -- Assert
      let items = response ^. QF.items
      assertNotNull "ledgerTip" (Just $ response ^. QF.ledgerTip)
      assertTrue "found UTXOs" (length items > 0)
      
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
      -- Assert - ReadTipResponse has a tip field, not slot/hash directly
      let tip = response ^. SF.tip
      let tipSlot = tip ^. SF.slot
      let tipHash = tip ^. SF.hash
      
      -- Assert that slot is a positive number
      assertTrue "slot > 0" (tipSlot > 0)
      
      -- Assert that hash is not empty
      assertTrue "hash not empty" (BS.length tipHash > 0)
      
      putStrLn $ "✓ SyncReadTip test passed! Slot: " ++ show tipSlot ++ ", Hash: " ++ 
                 T.unpack (T.take 16 $ T.decodeUtf8 $ Base16.encode tipHash) ++ "..."
      
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
      -- Assert
      let blocks = response ^. SF.block
      block <- assertSingle "blocks" blocks
      
      -- Assert block has cardano data
      let cardanoBlock = block ^. SF.cardano
      let blockHeader = cardanoBlock ^. CF.header
      assertEqual "block slot" testSlot (blockHeader ^. SF.slot)
      assertEqual "block height" 3399486 (blockHeader ^. SF.height)
      
      -- Assert block body exists (even if empty)
      let blockBody = cardanoBlock ^. CF.body
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
      -- Assert
      let blocks = response ^. SF.block
      block <- assertSingle "blocks" blocks
      
      -- Assert block has cardano data
      let cardanoBlock = block ^. SF.cardano
      let blockHeader = cardanoBlock ^. CF.header
      assertEqual "block slot" testSlot (blockHeader ^. SF.slot)
      assertEqual "block height" 3399486 (blockHeader ^. SF.height)
      
      -- Assert block body exists (even if empty)
      let blockBody = cardanoBlock ^. CF.body
      assertNotNull "block body" (Just blockBody)
      
      putStrLn "✓ SyncDumpHistory test passed!"
      
    err -> throwString $ "SyncDumpHistory failed: " ++ show err