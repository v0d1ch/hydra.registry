module Db.Schema where

import Data.Aeson (Value)
import Data.Functor.Identity (Identity)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Rel8

-- | Registered Hydra heads
data Head f = Head
  { headId :: Column f Text
  , headHost :: Column f Text
  , headPort :: Column f Int32
  , headStatus :: Column f Text
  , snapshotNumber :: Column f Int32
  , createdAt :: Column f UTCTime
  , updatedAt :: Column f UTCTime
  , lastMessageAt :: Column f (Maybe UTCTime)
  , headIsBridge :: Column f Bool
  , headBridgeFeeLovelace :: Column f (Maybe Int64)
  , -- | @"txhash#ix"@ pointing at an L2 UTxO inside this head that
    -- carries the HTLC validator as an inline reference script. When
    -- set, lock outputs no longer need to inline the validator (≈2 ADA
    -- min-ada instead of ≈5.6) and claim/refund txs spend the script
    -- via @--spending-tx-in-reference@. Operators publish the UTxO
    -- once and register it via @POST /heads/{id}/ref-script@.
    headRefScriptUtxo :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (Head Identity)
deriving stock instance Eq (Head Identity)

headSchema :: TableSchema (Head Name)
headSchema =
  TableSchema
    { name = "heads"
    , columns =
        Head
          { headId = "head_id"
          , headHost = "host"
          , headPort = "port"
          , headStatus = "status"
          , snapshotNumber = "snapshot_number"
          , createdAt = "created_at"
          , updatedAt = "updated_at"
          , lastMessageAt = "last_message_at"
          , headIsBridge = "is_bridge"
          , headBridgeFeeLovelace = "bridge_fee_lovelace"
          , headRefScriptUtxo = "ref_script_utxo"
          }
    }

-- | Explorer heads discovered from hydra-explorer (on-chain data)
data ExplorerHead f = ExplorerHead
  { explorerHeadId :: Column f Text
  , explorerNetwork :: Column f Text
  , explorerNetworkMagic :: Column f Int32
  , explorerVersion :: Column f Text
  , explorerStatus :: Column f Text
  , explorerContestationPeriod :: Column f (Maybe Int32)
  , explorerContestations :: Column f (Maybe Int32)
  , explorerSnapshotNumber :: Column f (Maybe Int32)
  , explorerContestationDeadline :: Column f (Maybe Text)
  , explorerPoint :: Column f (Maybe Value)
  , explorerBlockNo :: Column f (Maybe Int64)
  , explorerMembers :: Column f (Maybe Value)
  , explorerSeedTxIn :: Column f (Maybe Text)
  , explorerFirstSeenAt :: Column f UTCTime
  , explorerLastUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (ExplorerHead Identity)
deriving stock instance Eq (ExplorerHead Identity)

explorerHeadSchema :: TableSchema (ExplorerHead Name)
explorerHeadSchema =
  TableSchema
    { name = "explorer_heads"
    , columns =
        ExplorerHead
          { explorerHeadId = "head_id"
          , explorerNetwork = "network"
          , explorerNetworkMagic = "network_magic"
          , explorerVersion = "version"
          , explorerStatus = "status"
          , explorerContestationPeriod = "contestation_period"
          , explorerContestations = "contestations"
          , explorerSnapshotNumber = "snapshot_number"
          , explorerContestationDeadline = "contestation_deadline"
          , explorerPoint = "point"
          , explorerBlockNo = "block_no"
          , explorerMembers = "members"
          , explorerSeedTxIn = "seed_tx_in"
          , explorerFirstSeenAt = "first_seen_at"
          , explorerLastUpdatedAt = "last_updated_at"
          }
    }

-- | Head participants extracted from explorer members data
data HeadParticipant f = HeadParticipant
  { participantHeadId :: Column f Text
  , participantAddress :: Column f Text
  , participantVkey :: Column f (Maybe Text)
  , participantOnChainId :: Column f (Maybe Text)
  , participantCommittedLovelace :: Column f Int64
  , participantCommittedTxRef :: Column f (Maybe Text)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (HeadParticipant Identity)
deriving stock instance Eq (HeadParticipant Identity)

headParticipantSchema :: TableSchema (HeadParticipant Name)
headParticipantSchema =
  TableSchema
    { name = "head_participants"
    , columns =
        HeadParticipant
          { participantHeadId = "head_id"
          , participantAddress = "address"
          , participantVkey = "vkey"
          , participantOnChainId = "on_chain_id"
          , participantCommittedLovelace = "committed_lovelace"
          , participantCommittedTxRef = "committed_tx_ref"
          }
    }

-- | Invoices for relay payment requests.
--
-- The receiver is identified by their 28-byte Cardano key hash (the pkh
-- of the hydra-node @--cardano-signing-key@). Routing matches it against
-- @head_participants@ to find the destination head, and the same key
-- signs the final HTLC claim. Where claimed funds ultimately land is
-- the receiver's choice at claim-tx build time (an output to their
-- wallet, or wherever).
data Invoice f = Invoice
  { invoiceId :: Column f Text
  , invoiceReceiverOnChainId :: Column f Text
  , invoicePaymentHash :: Column f Text
  , invoiceAmountLovelace :: Column f Int64
  , invoiceMemo :: Column f (Maybe Text)
  , invoiceStatus :: Column f Text
  , invoiceExpiresAt :: Column f UTCTime
  , invoiceCreatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (Invoice Identity)
deriving stock instance Eq (Invoice Identity)

invoiceSchema :: TableSchema (Invoice Name)
invoiceSchema =
  TableSchema
    { name = "invoices"
    , columns =
        Invoice
          { invoiceId = "invoice_id"
          , invoiceReceiverOnChainId = "receiver_on_chain_id"
          , invoicePaymentHash = "payment_hash"
          , invoiceAmountLovelace = "amount_lovelace"
          , invoiceMemo = "memo"
          , invoiceStatus = "status"
          , invoiceExpiresAt = "expires_at"
          , invoiceCreatedAt = "created_at"
          }
    }

-- | Payment routes (a chosen path for a payment)
data PaymentRoute f = PaymentRoute
  { routeId :: Column f Text
  , routeInvoiceId :: Column f Text
  , routeSenderAddress :: Column f Text
  , routeReceiverAddress :: Column f Text
  , routeAmountLovelace :: Column f Int64
  , routeStatus :: Column f Text
  , routePath :: Column f Value
  , routeTotalFee :: Column f Int64
  , routeNetwork :: Column f Text
  , routeCreatedAt :: Column f UTCTime
  , routeUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (PaymentRoute Identity)
deriving stock instance Eq (PaymentRoute Identity)

paymentRouteSchema :: TableSchema (PaymentRoute Name)
paymentRouteSchema =
  TableSchema
    { name = "payment_routes"
    , columns =
        PaymentRoute
          { routeId = "route_id"
          , routeInvoiceId = "invoice_id"
          , routeSenderAddress = "sender_address"
          , routeReceiverAddress = "receiver_address"
          , routeAmountLovelace = "amount_lovelace"
          , routeStatus = "status"
          , routePath = "route_path"
          , routeTotalFee = "total_fee"
          , routeNetwork = "network"
          , routeCreatedAt = "created_at"
          , routeUpdatedAt = "updated_at"
          }
    }

-- | Individual hops within a payment route
data RouteHop f = RouteHop
  { hopId :: Column f Text
  , hopRouteId :: Column f Text
  , hopIndex :: Column f Int32
  , hopHeadId :: Column f Text
  , hopBridgeAddress :: Column f Text
  , hopSenderAddress :: Column f Text
  , hopReceiverAddress :: Column f Text
  , hopHtlcStatus :: Column f Text
  , hopHtlcTxHash :: Column f (Maybe Text)
  , hopSecretHash :: Column f Text
  , hopPreimage :: Column f (Maybe Text)
  , hopTimeoutSlot :: Column f Int64
  , hopFeeLovelace :: Column f Int64
  , hopLockedAt :: Column f (Maybe UTCTime)
  , hopClaimedAt :: Column f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance Show (RouteHop Identity)
deriving stock instance Eq (RouteHop Identity)

routeHopSchema :: TableSchema (RouteHop Name)
routeHopSchema =
  TableSchema
    { name = "route_hops"
    , columns =
        RouteHop
          { hopId = "hop_id"
          , hopRouteId = "route_id"
          , hopIndex = "hop_index"
          , hopHeadId = "head_id"
          , hopBridgeAddress = "bridge_address"
          , hopSenderAddress = "sender_address"
          , hopReceiverAddress = "receiver_address"
          , hopHtlcStatus = "htlc_status"
          , hopHtlcTxHash = "htlc_tx_hash"
          , hopSecretHash = "secret_hash"
          , hopPreimage = "preimage"
          , hopTimeoutSlot = "timeout_slot"
          , hopFeeLovelace = "fee_lovelace"
          , hopLockedAt = "locked_at"
          , hopClaimedAt = "claimed_at"
          }
    }

-- | UTxO entries indexed from Hydra heads
data Utxo f = Utxo
  { utxoTxHash :: Column f Text
  , utxoOutputIndex :: Column f Int32
  , utxoHeadId :: Column f Text
  , utxoAddress :: Column f Text
  , utxoLovelace :: Column f Int64
  , utxoAssets :: Column f Value
  , utxoDatumHash :: Column f (Maybe Text)
  , utxoInlineDatum :: Column f (Maybe Value)
  , utxoReferenceScriptHash :: Column f (Maybe Text)
  , utxoUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

utxoSchema :: TableSchema (Utxo Name)
utxoSchema =
  TableSchema
    { name = "utxos"
    , columns =
        Utxo
          { utxoTxHash = "tx_hash"
          , utxoOutputIndex = "output_index"
          , utxoHeadId = "head_id"
          , utxoAddress = "address"
          , utxoLovelace = "lovelace"
          , utxoAssets = "assets"
          , utxoDatumHash = "datum_hash"
          , utxoInlineDatum = "inline_datum"
          , utxoReferenceScriptHash = "reference_script_hash"
          , utxoUpdatedAt = "updated_at"
          }
    }
