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
