module Explorer.Members where

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Int (Int64)
import Data.Text (Text)
import Data.Vector qualified as V

-- | A parsed participant from the hydra-explorer members JSON
data ParsedParticipant = ParsedParticipant
  { address :: Text
  , vkey :: Maybe Text
  , onChainId :: Maybe Text
  , committedLovelace :: Int64
  , committedTxRef :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Convert a ParsedParticipant to a tuple for DB insertion
participantToTuple :: ParsedParticipant -> (Text, Maybe Text, Maybe Text, Int64, Maybe Text)
participantToTuple p = (p.address, p.vkey, p.onChainId, p.committedLovelace, p.committedTxRef)

-- | Get the address from a ParsedParticipant
participantAddress :: ParsedParticipant -> Text
participantAddress p = p.address

-- | Parse the members JSON blob from hydra-explorer into structured participants.
--
-- The expected shape is:
-- [
--   {
--     "party": {"vkey": "..."},
--     "onChainId": "...",
--     "commits": {
--       "txHash#index": {
--         "address": "addr_...",
--         "value": {"lovelace": 12345}
--       }
--     }
--   }
-- ]
parseMembers :: Maybe Value -> [ParsedParticipant]
parseMembers Nothing = []
parseMembers (Just (Array arr)) = concatMap parseMember (V.toList arr)
parseMembers _ = []

parseMember :: Value -> [ParsedParticipant]
parseMember (Object obj) =
  let vk = do
        Object party <- KM.lookup "party" obj
        String v <- KM.lookup "vkey" party
        pure v
      onChain = case KM.lookup "onChainId" obj of
        Just (String s) -> Just s
        _ -> Nothing
      commits = case KM.lookup "commits" obj of
        Just (Object cm) -> KM.toList cm
        _ -> []
   in [ ParsedParticipant
          { address = addr
          , vkey = vk
          , onChainId = onChain
          , committedLovelace = lovelace
          , committedTxRef = Just (Key.toText txRef)
          }
      | (txRef, commitVal) <- commits
      , Just (addr, lovelace) <- [parseCommitEntry commitVal]
      ]
parseMember _ = []

parseCommitEntry :: Value -> Maybe (Text, Int64)
parseCommitEntry (Object obj) = do
  String addr <- KM.lookup "address" obj
  Object val <- KM.lookup "value" obj
  Number n <- KM.lookup "lovelace" val
  pure (addr, round n)
parseCommitEntry _ = Nothing
