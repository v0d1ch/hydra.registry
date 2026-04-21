module Hydra.Deposit where

import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Term qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Exception (SomeException, try)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTP
import Network.HTTP.Types.Status (statusCode)

-- | Network-specific configuration for the deposit reference UTxO
data DepositConfig = DepositConfig
  { txRef :: Text
  , alwaysTrueAddress :: Text
  }

-- | Get deposit config for a given network
depositConfigForNetwork :: Text -> Maybe DepositConfig
depositConfigForNetwork "Preview" =
  Just
    DepositConfig
      { txRef = "caaa5194116c2dc1c9f738cef6218c6bcf4a59937c220660ebb7d386a91a234e#0"
      , alwaysTrueAddress = "addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8"
      }
depositConfigForNetwork "Preprod" =
  Just
    DepositConfig
      { txRef = "dd5b89fec6679046ddb17377e070ff306a120eb5f496ff8b9dff05a4e904ba63#0"
      , alwaysTrueAddress = "addr_test1wr2yzgn42ws0r2t9lmnavzs0wf9ndrw3hhduyzrnplxwhncaya5f8"
      }
depositConfigForNetwork _ = Nothing

-- | Orchestrate the full deposit flow:
--   1. Build commit payload with HTLC reference script
--   2. POST to Hydra node /commit endpoint
--   3. Patch the returned tx CBOR to include always-true native script witness
requestDeposit :: Text -> Int -> Text -> Text -> IO (Either Text Text)
requestDeposit host port network htlcScriptCbor = do
  case depositConfigForNetwork network of
    Nothing -> pure $ Left $ "Unsupported network: " <> network
    Just cfg -> do
      let payload = buildCommitPayload cfg htlcScriptCbor
      result <- callCommitEndpoint host port payload
      case result of
        Left err -> pure $ Left err
        Right txCborHex -> do
          case patchDepositTx txCborHex of
            Left err -> pure $ Left $ "Failed to patch deposit tx: " <> err
            Right patched -> pure $ Right patched

-- | Build the JSON payload for the Hydra /commit endpoint
buildCommitPayload :: DepositConfig -> Text -> Aeson.Value
buildCommitPayload cfg htlcScriptCbor =
  Aeson.Object $
    KM.singleton (Key.fromText cfg.txRef) $
      Aeson.object
        [ "address" Aeson..= cfg.alwaysTrueAddress
        , "value" Aeson..= Aeson.object ["lovelace" Aeson..= (6000000 :: Int)]
        , "referenceScript"
            Aeson..= Aeson.object
              [ "script"
                  Aeson..= Aeson.object
                    [ "cborHex" Aeson..= htlcScriptCbor
                    , "description" Aeson..= ("" :: Text)
                    , "type" Aeson..= ("PlutusScriptV3" :: Text)
                    ]
              , "type" Aeson..= ("PlutusV3" :: Text)
              ]
        ]

-- | POST to the Hydra node's /commit endpoint, return the tx CBOR hex
callCommitEndpoint :: Text -> Int -> Aeson.Value -> IO (Either Text Text)
callCommitEndpoint host port payload = do
  result <- try @SomeException $ do
    manager <- HTTP.newTlsManager
    let url = "http://" <> T.unpack host <> ":" <> show port <> "/commit"
    initReq <- HTTP.parseRequest url
    let req =
          initReq
            { HTTP.method = "POST"
            , HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode payload)
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }
    resp <- HTTP.httpLbs req manager
    let status = HTTP.responseStatus resp
    if statusCode status >= 200 && statusCode status < 300
      then case Aeson.decode (HTTP.responseBody resp) of
        Just (Aeson.Object obj) -> case KM.lookup "cborHex" obj of
          Just (Aeson.String cbor) -> pure $ Right cbor
          _ -> case KM.lookup "txCbor" obj of
            Just (Aeson.String cbor) -> pure $ Right cbor
            _ ->
              -- Try treating the whole body as the CBOR hex text
              pure $ Right $ TE.decodeUtf8 $ LBS.toStrict $ HTTP.responseBody resp
        _ ->
          pure $ Right $ TE.decodeUtf8 $ LBS.toStrict $ HTTP.responseBody resp
      else
        pure $ Left $ "Hydra /commit returned status " <> T.pack (show (statusCode status)) <> " (url: " <> T.pack url <> "): " <> TE.decodeUtf8 (LBS.toStrict (HTTP.responseBody resp))
  case result of
    Left err -> pure $ Left $ "HTTP error calling /commit: " <> T.pack (show err)
    Right r -> pure r

-- | Patch a Conway-era transaction CBOR to inject the always-true native script witness.
--
-- Transaction structure: 84 [body, witness_set, is_valid, auxiliary_data]
-- Witness set may be wrapped in CBOR tag 258 (d90102).
-- We add key 1 -> [[0, []]] (native script = ScriptAll []) to the witness map.
patchDepositTx :: Text -> Either Text Text
patchDepositTx hexCbor = do
  -- Strip any surrounding quotes/whitespace
  let cleanHex = T.strip hexCbor
  rawBytes <- case Base16.decode (TE.encodeUtf8 cleanHex) of
    Left err -> Left $ "Invalid hex: " <> T.pack err
    Right bs -> Right bs
  -- Decode CBOR
  term <- case CBOR.deserialiseFromBytes CBOR.decodeTerm (LBS.fromStrict rawBytes) of
    Left err -> Left $ "CBOR decode error: " <> T.pack (show err)
    Right (_, t) -> Right t
  -- Navigate the transaction array
  case term of
    CBOR.TList [body, witnessSet, isValid, auxData] -> do
      patchedWitness <- patchWitnessSet witnessSet
      let patchedTx = CBOR.TList [body, patchedWitness, isValid, auxData]
          encoded = CBOR.toLazyByteString (CBOR.encodeTerm patchedTx)
      Right $ TE.decodeUtf8 $ Base16.encode $ LBS.toStrict encoded
    CBOR.TListI [body, witnessSet, isValid, auxData] -> do
      patchedWitness <- patchWitnessSet witnessSet
      let patchedTx = CBOR.TList [body, patchedWitness, isValid, auxData]
          encoded = CBOR.toLazyByteString (CBOR.encodeTerm patchedTx)
      Right $ TE.decodeUtf8 $ Base16.encode $ LBS.toStrict encoded
    _ -> Left "Transaction CBOR is not a 4-element array"

-- | Patch the witness set to include the always-true native script.
-- The always-true script is ScriptAll [] = [0, []] in CBOR.
-- Native scripts go under key 1 in the witness set map.
patchWitnessSet :: CBOR.Term -> Either Text CBOR.Term
patchWitnessSet term = case term of
  -- Tagged with 258 (d90102) — common Hydra encoding
  CBOR.TTagged 258 inner -> do
    patched <- patchWitnessMap inner
    Right $ CBOR.TTagged 258 patched
  -- Untagged map
  CBOR.TMap kvs -> do
    Right $ CBOR.TMap $ addNativeScript kvs
  CBOR.TMapI kvs -> do
    Right $ CBOR.TMap $ addNativeScript kvs
  other -> Left $ "Unexpected witness set structure: " <> T.pack (show other)

patchWitnessMap :: CBOR.Term -> Either Text CBOR.Term
patchWitnessMap (CBOR.TMap kvs) = Right $ CBOR.TMap $ addNativeScript kvs
patchWitnessMap (CBOR.TMapI kvs) = Right $ CBOR.TMap $ addNativeScript kvs
patchWitnessMap other = Left $ "Witness set inner is not a map: " <> T.pack (show other)

-- | Add the native script entry (key 1) to a witness set map.
-- If key 1 already exists, append our script to the existing array.
addNativeScript :: [(CBOR.Term, CBOR.Term)] -> [(CBOR.Term, CBOR.Term)]
addNativeScript kvs =
  let key1 = CBOR.TInt 1
      alwaysTrueScript = CBOR.TList [CBOR.TInt 0, CBOR.TList []]
      -- Check if key 1 already exists
      hasKey1 = any (\(k, _) -> k == key1) kvs
   in if hasKey1
        then
          map
            ( \(k, v) ->
                if k == key1
                  then case v of
                    CBOR.TList scripts -> (k, CBOR.TList (scripts ++ [alwaysTrueScript]))
                    _ -> (k, CBOR.TList [alwaysTrueScript])
                  else (k, v)
            )
            kvs
        else kvs ++ [(key1, CBOR.TList [alwaysTrueScript])]

-- | Extract a reference script hash from the Hydra referenceScript JSON.
-- Tries:
--   1. .hash field directly
--   2. .script.cborHex field -> hash with blake2b-224
-- For now, we look for "hash" field only (blake2b requires extra dependency).
extractReferenceScriptHash :: Aeson.Value -> Maybe Text
extractReferenceScriptHash val = case val of
  Aeson.Object obj ->
    -- Try direct hash field
    case KM.lookup "hash" obj of
      Just (Aeson.String h) -> Just h
      _ -> case KM.lookup "script" obj of
        Just (Aeson.Object scriptObj) ->
          case KM.lookup "cborHex" scriptObj of
            Just (Aeson.String _cborHex) ->
              -- We would need blake2b-224 to hash this.
              -- For now, return Nothing and rely on the HYDRA_HTLC_SCRIPT_HASH env var.
              Nothing
            _ -> Nothing
        _ -> Nothing
  _ -> Nothing
