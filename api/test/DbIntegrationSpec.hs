module DbIntegrationSpec (spec) where

import Control.Exception (SomeException, try)
import Data.Maybe (isJust)
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import Db qualified
import Db.Schema (AgentRegistration (..), ExplorerHead (..), Head (..), Invoice (..))
import Hasql.Pool (Pool)
import Hydra.Client (HydraUtxoEntry (..))
import Test.Hspec
import TestUtils

spec :: Spec
spec = describe "Db (integration)" $ around withTestPool $ do
  describe "initDb" $ do
    it "creates tables without error (idempotent)" $ \pool -> do
      Db.initDb pool

  describe "upsertHead" $ do
    it "inserts a new head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found after insert"
        Just Head{headId, headHost, headStatus} -> do
          headId `shouldBe` ("head-1" :: Text)
          headHost `shouldBe` ("localhost" :: Text)
          headStatus `shouldBe` ("Open" :: Text)

    it "updates existing head on conflict" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.upsertHead pool "head-1" "localhost" 4001 "Closed" Nothing
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found after upsert"
        Just Head{headId} ->
          headId `shouldBe` ("head-1" :: Text)

    it "stores registered_by wallet address" $ \pool -> do
      Db.setUserKeyHash pool "addr1alice" "aabbcc"
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" (Just "addr1alice")
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found"
        Just h -> h.headRegisteredBy `shouldBe` Just "addr1alice"

    it "registered_by is preserved on upsert conflict" $ \pool -> do
      Db.setUserKeyHash pool "addr1alice" "aabbcc"
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" (Just "addr1alice")
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found"
        Just h -> h.headRegisteredBy `shouldBe` Just "addr1alice"

  describe "getHeadsByWallet" $ do
    it "returns heads registered by a specific wallet" $ \pool -> do
      Db.setUserKeyHash pool "addr1alice" "aabbcc"
      Db.setUserKeyHash pool "addr1bob"   "ddeeff"
      Db.upsertHead pool "head-1" "localhost" 4001 "Open"   (Just "addr1alice")
      Db.upsertHead pool "head-2" "localhost" 4002 "Open"   (Just "addr1alice")
      Db.upsertHead pool "head-3" "localhost" 4003 "Closed" (Just "addr1bob")
      aliceHeads <- Db.getHeadsByWallet pool "addr1alice"
      length aliceHeads `shouldBe` 2
      bobHeads <- Db.getHeadsByWallet pool "addr1bob"
      length bobHeads `shouldBe` 1

    it "returns empty list for unknown wallet" $ \pool -> do
      heads <- Db.getHeadsByWallet pool "addr1unknown"
      length heads `shouldBe` 0

    it "does not return heads with no registered_by" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      heads <- Db.getHeadsByWallet pool "addr1alice"
      length heads `shouldBe` 0

  describe "updateHeadStatus" $ do
    it "changes status of an existing head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.updateHeadStatus pool "head-1" "Closed"
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found"
        Just Head{headStatus} ->
          headStatus `shouldBe` ("Closed" :: Text)

  describe "getAllHeads" $ do
    it "returns all registered heads" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.upsertHead pool "head-2" "localhost" 4002 "Closed" Nothing
      heads <- Db.getAllHeads pool
      length heads `shouldBe` 2

  describe "getAllHeadsPaginated" $ do
    it "returns paginated results" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.upsertHead pool "head-2" "localhost" 4002 "Open" Nothing
      Db.upsertHead pool "head-3" "localhost" 4003 "Open" Nothing
      page1 <- Db.getAllHeadsPaginated pool 2 1
      length page1 `shouldBe` 2
      page2 <- Db.getAllHeadsPaginated pool 2 2
      length page2 `shouldBe` 1

  describe "getHead" $ do
    it "returns Nothing for non-existent head" $ \pool -> do
      mHead <- Db.getHead pool "non-existent"
      mHead `shouldBe` Nothing

    it "returns the head when it exists" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Expected head to exist"
        Just _ -> pure ()

  describe "replaceUtxos" $ do
    it "stores UTxOs for a head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 1

    it "replaces existing UTxOs" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      let newEntry = sampleUtxoEntry{txHash = "newtx123", lovelace = 10_000_000}
      Db.replaceUtxos pool "head-1" [newEntry]
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 1

    it "handles empty UTxO list" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      Db.replaceUtxos pool "head-1" []
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 0

  describe "getUtxosByAddressFlat" $ do
    it "returns UTxOs from all heads" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.upsertHead pool "head-2" "localhost" 4002 "Open" Nothing
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      Db.replaceUtxos pool "head-2" [sampleUtxoEntry{txHash = "other-tx"}]
      results <- Db.getUtxosByAddressFlat pool "addr1qxtest" 100 1
      length results `shouldBe` 2

  describe "deleteUtxosForHead" $ do
    it "removes all UTxOs for a head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      Db.deleteUtxosForHead pool "head-1"
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 0

  describe "deleteHead" $ do
    it "removes head and its UTxOs (cascade)" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.replaceUtxos pool "head-1" [sampleUtxoEntry]
      Db.deleteHead pool "head-1"
      mHead <- Db.getHead pool "head-1"
      mHead `shouldBe` Nothing
      utxos <- Db.getUtxosByAddressAndHead pool "head-1" "addr1qxtest"
      length utxos `shouldBe` 0

  describe "countUtxosForHead" $ do
    it "counts UTxOs correctly" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      let entries =
            [ sampleUtxoEntry
            , sampleUtxoEntry{txHash = "tx2", outputIndex = 1}
            , sampleUtxoEntry{txHash = "tx3", outputIndex = 0, address = "addr1other"}
            ]
      Db.replaceUtxos pool "head-1" entries
      count <- Db.countUtxosForHead pool "head-1"
      count `shouldBe` 3

  describe "getAddressesForHead" $ do
    it "returns distinct addresses" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      let entries =
            [ sampleUtxoEntry
            , sampleUtxoEntry{txHash = "tx2", outputIndex = 1}
            , sampleUtxoEntry{txHash = "tx3", address = "addr1other"}
            ]
      Db.replaceUtxos pool "head-1" entries
      addrs <- Db.getAddressesForHead pool "head-1"
      length addrs `shouldBe` 2

  describe "getBalanceForAddressInHead" $ do
    it "aggregates lovelace across UTxOs" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      let entries =
            [ sampleUtxoEntry{lovelace = 3_000_000}
            , sampleUtxoEntry{txHash = "tx2", outputIndex = 1, lovelace = 7_000_000}
            ]
      Db.replaceUtxos pool "head-1" entries
      (total, _) <- Db.getBalanceForAddressInHead pool "head-1" "addr1qxtest"
      total `shouldBe` (10_000_000 :: Int64)

  describe "checkDbConnectivity" $ do
    it "returns True when DB is reachable" $ \pool -> do
      result <- Db.checkDbConnectivity pool
      result `shouldBe` True

  describe "Explorer heads" $ do
    describe "upsertExplorerHead" $ do
      it "inserts a new explorer head" $ \pool -> do
        Db.upsertExplorerHead pool "explorer-1" "Mainnet" 764824073 "0.21.0" "Open"
          (Just 60) Nothing (Just 5) Nothing Nothing Nothing Nothing Nothing
        mHead <- Db.getExplorerHead pool "explorer-1"
        case mHead of
          Nothing -> expectationFailure "Explorer head not found after insert"
          Just ExplorerHead{explorerHeadId, explorerNetwork, explorerStatus} -> do
            explorerHeadId `shouldBe` ("explorer-1" :: Text)
            explorerNetwork `shouldBe` ("Mainnet" :: Text)
            explorerStatus `shouldBe` ("Open" :: Text)

      it "updates existing explorer head on conflict (preserves firstSeenAt)" $ \pool -> do
        Db.upsertExplorerHead pool "explorer-1" "Mainnet" 764824073 "0.21.0" "Open"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        mHead1 <- Db.getExplorerHead pool "explorer-1"
        Db.upsertExplorerHead pool "explorer-1" "Mainnet" 764824073 "0.21.0" "Closed"
          Nothing (Just 1) (Just 10) Nothing Nothing Nothing Nothing Nothing
        mHead2 <- Db.getExplorerHead pool "explorer-1"
        case (mHead1, mHead2) of
          (Just h1, Just h2) -> do
            h2.explorerStatus `shouldBe` ("Closed" :: Text)
            h2.explorerFirstSeenAt `shouldBe` h1.explorerFirstSeenAt
          _ -> expectationFailure "Explorer heads not found"

    describe "getAllExplorerHeads" $ do
      it "returns all explorer heads" $ \pool -> do
        Db.upsertExplorerHead pool "e-1" "Mainnet" 764824073 "0.21.0" "Open"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "e-2" "Testnet" 1 "0.20.0" "Finalized"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        heads <- Db.getAllExplorerHeads pool
        length heads `shouldBe` 2

    describe "getExplorerHeadsPaginated" $ do
      it "returns paginated results" $ \pool -> do
        Db.upsertExplorerHead pool "e-1" "Mainnet" 764824073 "0.21.0" "Open"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "e-2" "Mainnet" 764824073 "0.21.0" "Closed"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "e-3" "Testnet" 1 "0.20.0" "Open"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        page1 <- Db.getExplorerHeadsPaginated pool 2 1 Nothing Nothing
        length page1 `shouldBe` 2
        page2 <- Db.getExplorerHeadsPaginated pool 2 2 Nothing Nothing
        length page2 `shouldBe` 1

      it "filters by status" $ \pool -> do
        Db.upsertExplorerHead pool "e-1" "Mainnet" 764824073 "0.21.0" "Open"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "e-2" "Mainnet" 764824073 "0.21.0" "Closed"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        open <- Db.getExplorerHeadsPaginated pool 100 1 (Just "Open") Nothing
        length open `shouldBe` 1
        (head open).explorerHeadId `shouldBe` ("e-1" :: Text)

      it "filters by network" $ \pool -> do
        Db.upsertExplorerHead pool "e-1" "Mainnet" 764824073 "0.21.0" "Open"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "e-2" "Testnet" 1 "0.20.0" "Open"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        mainnet <- Db.getExplorerHeadsPaginated pool 100 1 Nothing (Just "Mainnet")
        length mainnet `shouldBe` 1
        (head mainnet).explorerNetwork `shouldBe` ("Mainnet" :: Text)

    describe "getExplorerHead" $ do
      it "returns Nothing for non-existent head" $ \pool -> do
        mHead <- Db.getExplorerHead pool "non-existent"
        mHead `shouldBe` Nothing

    describe "countExplorerHeads" $ do
      it "counts correctly" $ \pool -> do
        Db.upsertExplorerHead pool "e-1" "Mainnet" 764824073 "0.21.0" "Open"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        Db.upsertExplorerHead pool "e-2" "Testnet" 1 "0.20.0" "Closed"
          Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        count <- Db.countExplorerHeads pool
        count `shouldBe` 2

  describe "countHeads" $ do
    it "returns 0 for empty registry" $ \pool -> do
      n <- Db.countHeads pool
      n `shouldBe` 0

    it "counts registered heads correctly" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.upsertHead pool "head-2" "localhost" 4002 "Open" Nothing
      Db.upsertHead pool "head-3" "localhost" 4003 "Open" Nothing
      n <- Db.countHeads pool
      n `shouldBe` 3

    it "does not double-count upserted head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.upsertHead pool "head-1" "localhost" 4001 "Closed" Nothing
      n <- Db.countHeads pool
      n `shouldBe` 1

  describe "unique (host, port) constraint" $ do
    it "rejects two different heads at the same host and port" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      result <- try @SomeException $ Db.upsertHead pool "head-2" "localhost" 4001 "Open" Nothing
      case result of
        Left _  -> pure ()
        Right _ -> expectationFailure "Expected unique-constraint violation for duplicate host:port"

    it "allows the same head to upsert (same headId, same host:port)" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.upsertHead pool "head-1" "localhost" 4001 "Closed" Nothing
      n <- Db.countHeads pool
      n `shouldBe` 1

  describe "getUserKeyHash / setUserKeyHash" $ do
    it "returns Nothing when no profile exists" $ \pool -> do
      result <- Db.getUserKeyHash pool "addr1_unknown"
      result `shouldBe` Nothing

    it "stores and retrieves a key hash" $ \pool -> do
      Db.setUserKeyHash pool "addr1qxtest" "deadbeef"
      result <- Db.getUserKeyHash pool "addr1qxtest"
      result `shouldBe` Just "deadbeef"

    it "updates key hash on second call (upsert)" $ \pool -> do
      Db.setUserKeyHash pool "addr1qxtest" "deadbeef"
      Db.setUserKeyHash pool "addr1qxtest" "cafebabe"
      result <- Db.getUserKeyHash pool "addr1qxtest"
      result `shouldBe` Just "cafebabe"

    it "stores independent profiles per wallet address" $ \pool -> do
      Db.setUserKeyHash pool "addr1alice" "aabbcc"
      Db.setUserKeyHash pool "addr1bob"   "ddeeff"
      rAlice <- Db.getUserKeyHash pool "addr1alice"
      rBob   <- Db.getUserKeyHash pool "addr1bob"
      rAlice `shouldBe` Just "aabbcc"
      rBob   `shouldBe` Just "ddeeff"

  describe "getInvoicesByReceiver" $ do
    it "returns empty list when no invoices exist" $ \pool -> do
      invoices <- Db.getInvoicesByReceiver pool "deadbeef"
      length invoices `shouldBe` 0

    it "returns invoices for matching receiver" $ \pool -> do
      future <- addUTCTime 3600 <$> getCurrentTime
      Db.insertInvoice pool "inv-1" "head-a" "alice-pkh" "hash1" 10_000_000 Nothing "pending" future
      Db.insertInvoice pool "inv-2" "head-a" "alice-pkh" "hash2" 20_000_000 Nothing "pending" future
      invoices <- Db.getInvoicesByReceiver pool "alice-pkh"
      length invoices `shouldBe` 2

    it "does not return invoices for other receivers" $ \pool -> do
      future <- addUTCTime 3600 <$> getCurrentTime
      Db.insertInvoice pool "inv-1" "head-a" "alice-pkh" "hash1" 10_000_000 Nothing "pending" future
      Db.insertInvoice pool "inv-2" "head-a" "bob-pkh"   "hash2" 10_000_000 Nothing "pending" future
      invoices <- Db.getInvoicesByReceiver pool "alice-pkh"
      length invoices `shouldBe` 1

    it "returns invoices of all statuses" $ \pool -> do
      future <- addUTCTime 3600 <$> getCurrentTime
      Db.insertInvoice pool "inv-1" "head-a" "alice-pkh" "hash1" 10_000_000 Nothing "pending" future
      Db.insertInvoice pool "inv-2" "head-a" "alice-pkh" "hash2" 10_000_000 Nothing "paid"    future
      Db.insertInvoice pool "inv-3" "head-a" "alice-pkh" "hash3" 10_000_000 Nothing "expired" future
      invoices <- Db.getInvoicesByReceiver pool "alice-pkh"
      length invoices `shouldBe` 3

  describe "getPendingInvoices" $ do
    it "returns empty list when no invoices exist" $ \pool -> do
      invoices <- Db.getPendingInvoices pool
      length invoices `shouldBe` 0

    it "returns only pending invoices across all receivers" $ \pool -> do
      future <- addUTCTime 3600 <$> getCurrentTime
      Db.insertInvoice pool "inv-1" "head-a" "alice-pkh" "hash1" 10_000_000 Nothing "pending" future
      Db.insertInvoice pool "inv-2" "head-a" "bob-pkh"   "hash2" 10_000_000 Nothing "pending" future
      Db.insertInvoice pool "inv-3" "head-a" "alice-pkh" "hash3" 10_000_000 Nothing "paid"    future
      invoices <- Db.getPendingInvoices pool
      length invoices `shouldBe` 2

    it "does not return paid or expired invoices" $ \pool -> do
      future <- addUTCTime 3600 <$> getCurrentTime
      Db.insertInvoice pool "inv-1" "head-a" "alice-pkh" "hash1" 10_000_000 Nothing "paid"    future
      Db.insertInvoice pool "inv-2" "head-a" "alice-pkh" "hash2" 10_000_000 Nothing "expired" future
      invoices <- Db.getPendingInvoices pool
      length invoices `shouldBe` 0

    it "stores and returns headId on the invoice" $ \pool -> do
      future <- addUTCTime 3600 <$> getCurrentTime
      Db.insertInvoice pool "inv-1" "head-xyz" "alice-pkh" "hash1" 5_000_000 Nothing "pending" future
      invoices <- Db.getPendingInvoices pool
      length invoices `shouldBe` 1
      let inv = invoices !! 0
      inv.invoiceHeadId `shouldBe` "head-xyz"

  describe "insertAgentRegistration / lookupAgentBySecretHash" $ do
    it "stores an agent registration and retrieves it by secret hash" $ \pool -> do
      Db.insertAgentRegistration pool "agent-1" "head-1" "secrethashABC" "sha256:binaryhashXYZ"
      mAgent <- Db.lookupAgentBySecretHash pool "secrethashABC"
      case mAgent of
        Nothing -> expectationFailure "Agent not found after insert"
        Just reg -> do
          reg.agentId       `shouldBe` "agent-1"
          reg.agentHeadId   `shouldBe` "head-1"
          reg.agentSecretHash `shouldBe` "secrethashABC"
          reg.agentBinaryHash `shouldBe` "sha256:binaryhashXYZ"

    it "returns Nothing for an unknown secret hash" $ \pool -> do
      mAgent <- Db.lookupAgentBySecretHash pool "nonexistent-hash"
      mAgent `shouldBe` Nothing

  describe "updateAgentLastSeen" $ do
    it "sets last_seen_at for a known agent" $ \pool -> do
      Db.insertAgentRegistration pool "agent-2" "head-1" "secrethashDEF" "sha256:binaryhash000"
      now <- getCurrentTime
      Db.updateAgentLastSeen pool "agent-2" now
      mAgent <- Db.lookupAgentBySecretHash pool "secrethashDEF"
      case mAgent of
        Nothing -> expectationFailure "Agent not found"
        Just reg -> reg.agentLastSeenAt `shouldSatisfy` isJust

  describe "setHeadRegisteredBy" $ do
    it "sets registered_by on an existing head" $ \pool -> do
      Db.upsertHead pool "head-1" "localhost" 4001 "Open" Nothing
      Db.setUserKeyHash pool "addr1alice" "aabbcc"
      Db.setHeadRegisteredBy pool "head-1" "addr1alice"
      mHead <- Db.getHead pool "head-1"
      case mHead of
        Nothing -> expectationFailure "Head not found"
        Just h -> h.headRegisteredBy `shouldBe` Just "addr1alice"
