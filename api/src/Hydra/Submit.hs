module Hydra.Submit
  ( submitToHead
  , SubmitResult (..)
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Data.Aeson (Value (..), eitherDecode, encode, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Hydra.Client (normalizeHost)
import Network.WebSockets qualified as WS

-- | Outcome of pushing a signed tx through a head's WS @NewTx@
-- envelope. The hydra-node validates against its L2 ledger and
-- replies with either @TxValid@ or @TxInvalid@ — we map directly.
--
-- 'SubmitTimeout' covers the "no decisive answer in @timeoutSeconds@"
-- case: typically a hung WS connection or a head that's not actually
-- open. Callers should treat it like @TxInvalid@ (the tx didn't
-- land) but with a different user-facing message.
data SubmitResult
  = SubmitValid {txId :: Text}
  | SubmitInvalid {reason :: Text}
  | SubmitTimeout
  | SubmitConnectError {reason :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

-- | Open a one-shot WS to @host:port@, send the tx as @NewTx@, and
-- block reading replies until either @TxValid@ or @TxInvalid@
-- arrives or 'timeoutSeconds' elapses. The connection is closed
-- on the way out regardless of outcome.
submitToHead :: Text -> Int -> Text -> IO SubmitResult
submitToHead host portNum signedCborHex = do
  let envelope =
        object
          [ "tag" .= ("NewTx" :: Text)
          ,
            ( "transaction"
            , object
                [ "type" .= ("Tx ConwayEra" :: Text)
                , "description" .= ("" :: Text)
                , "cborHex" .= signedCborHex
                ]
            )
          ]
  outcome <-
    race
      (timeoutDelay timeoutSeconds)
      ( try @SomeException $
          WS.runClient
            (T.unpack (normalizeHost host))
            portNum
            "/"
            (\conn -> sendAndAwait conn (encode envelope))
      )
  case outcome of
    Left () -> pure SubmitTimeout
    Right (Left e) -> pure SubmitConnectError{reason = T.pack (show e)}
    Right (Right r) -> pure r

-- | How long to wait for a TxValid/TxInvalid before giving up.
timeoutSeconds :: Int
timeoutSeconds = 15

timeoutDelay :: Int -> IO ()
timeoutDelay s = threadDelay (s * 1_000_000)

-- | Send the @NewTx@ envelope and read messages from the WS until
-- we see a verdict. The hydra-node sends a Greetings on connect and
-- may interleave snapshot updates etc; we ignore everything except
-- @TxValid@/@TxInvalid@. Bounded read budget so a chatty head
-- doesn't keep us looping forever.
sendAndAwait :: WS.Connection -> BSL.ByteString -> IO SubmitResult
sendAndAwait conn payload = do
  WS.sendTextData conn payload
  let loop budget
        | budget <= 0 = pure SubmitTimeout
        | otherwise = do
            msg <- WS.receiveData conn
            case eitherDecode msg :: Either String Value of
              Left _ -> loop (budget - 1)
              Right v -> case classify v of
                Just r -> pure r
                Nothing -> loop (budget - 1)
  loop maxMessages

-- | Upper bound on messages we'll read while waiting for a verdict.
-- Each is small but a head emitting many snapshot updates can
-- starve us; 200 is generous and bounded.
maxMessages :: Int
maxMessages = 200

-- | Inspect a parsed WS message; return a 'SubmitResult' iff it's
-- one of the verdict messages we care about, else 'Nothing' so the
-- caller keeps reading.
classify :: Value -> Maybe SubmitResult
classify (Object o) = case lookupTag o of
  Just "TxValid" -> Just SubmitValid{txId = lookupTxId o}
  Just "TxInvalid" -> Just SubmitInvalid{reason = lookupReason o}
  _ -> Nothing
classify _ = Nothing

lookupTag :: KM.KeyMap Value -> Maybe Text
lookupTag o = case KM.lookup "tag" o of
  Just (String s) -> Just s
  _ -> Nothing

lookupTxId :: KM.KeyMap Value -> Text
lookupTxId o = case KM.lookup "transaction" o of
  Just (Object t) -> case KM.lookup "txId" t of
    Just (String s) -> s
    _ -> ""
  _ -> ""

lookupReason :: KM.KeyMap Value -> Text
lookupReason o = case KM.lookup "validationError" o of
  Just (Object t) -> case KM.lookup "reason" t of
    Just (String s) -> s
    _ -> ""
  Just (String s) -> s
  _ -> ""
