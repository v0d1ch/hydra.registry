{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Common.Api where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

commonStuff :: String
commonStuff = "Hydra Head Registry is public registry of Hydra heads. In order to get your Head listed send email to sasa [dot] bogicevic [at] pm [dot] me and specify <IP:PORT> for your Hydra Head"

data HeadStatus
  = Idle
  | Initializing
  | Open
  | Closed
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

data ApiMsg = ApiMsg
  { _apiMsg_headId :: Text -- TODO: use the real type from Hydra here eventually
  , _apiMsg_status :: HeadStatus
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
