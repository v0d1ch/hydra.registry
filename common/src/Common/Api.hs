{-# LANGUAGE DerivingStrategies #-}

module Common.Api where

import Data.Text (Text)

commonStuff :: String
commonStuff = "Hydra Head Registry is public registry of Hydra heads. In order to get your Head listed please submit a PR to the github repository (https://github.com/v0d1ch/hydra.registry) and add <IP:PORT> to the list of Heads at https://github.com/v0d1ch/hydra.registry/blob/main/config/common/heads or send an email to sasa [dot] bogicevic [at] pm [dot] me"

data HeadStatus
  = Idle
  | Initializing
  | Open
  | Closed
  deriving stock (Show, Eq)

data ApiMsg = ApiMsg
  { _apiMsg_headId :: Text -- TODO: use the real type from Hydra here eventually
  , _apiMsg_status :: HeadStatus
  }
  deriving stock (Show, Eq)
