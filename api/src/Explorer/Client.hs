module Explorer.Client where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A single head as returned by the hydra-explorer API
data ExplorerHeadEntry = ExplorerHeadEntry
  { headId :: Text
  , network :: Text
  , networkMagic :: Int
  , version :: Text
  , status :: Text
  , contestationPeriod :: Maybe Int
  , contestations :: Maybe Int
  , snapshotNumber :: Maybe Int
  , contestationDeadline :: Maybe Text
  , point :: Maybe Value
  , blockNo :: Maybe Int
  , members :: Maybe Value
  , seedTxIn :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON ExplorerHeadEntry where
  parseJSON = withObject "ExplorerHeadEntry" $ \v ->
    ExplorerHeadEntry
      <$> v .: "headId"
      <*> v .: "network"
      <*> v .: "networkMagic"
      <*> v .: "version"
      <*> v .: "status"
      <*> v .:? "contestationPeriod"
      <*> v .:? "contestations"
      <*> v .:? "snapshotNumber"
      <*> v .:? "contestationDeadline"
      <*> v .:? "point"
      <*> v .:? "blockNo"
      <*> v .:? "members"
      <*> v .:? "seedTxIn"

instance ToJSON ExplorerHeadEntry where
  toJSON = genericToJSON defaultOptions
