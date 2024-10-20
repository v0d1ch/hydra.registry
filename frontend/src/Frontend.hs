{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Frontend where

import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Configs
import Obelisk.Frontend
import Obelisk.Generated.Static
import Obelisk.Route
import Reflex.Dom.Core

import Control.Monad.Fix (MonadFix)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, _String)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (forM_)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Lens.Micro ((^?))

import Common.Api
import Common.Route

default (T.Text)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend =
  Frontend
    { _frontend_head = do
        el "title" $ text "Hydra Head Registry"
        elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no") blank

        elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.googleapis.com") blank
        elAttr "link" ("rel" =: "preconnect" <> "href" =: "https://fonts.gstatic.com") blank
        elAttr "link" ("href" =: "https://fonts.googleapis.com/css2?family=Inria+Sans:wght@300&family=Inter:wght@100;200;300;400;500;600;700;800;900&family=Krona+One&family=Rajdhani:wght@300;400;500;600;700&display=swap" <> "rel" =: "stylesheet") blank
        elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://fonts.googleapis.com/css2?family=Material+Symbols+Rounded:opsz,wght,FILL,GRAD@48,400,0,0") blank
        elAttr "script" ("src" =: "https://cdn.tailwindcss.com") blank

        -- Highlight JS for syntax highlighting
        elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/styles/default.min.css") blank
        elAttr "script" ("src" =: "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.6.0/highlight.min.js") blank
        elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
        elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
    , _frontend_body = do
        elClass "h1" "mdcTypography" $ text "Hydra Head Registry"
        el "p" $ text $ T.pack commonStuff
        prerender_ (text "Loading Heads...") $ do
          pb <- getPostBuild
          let
            allHeads = "common/heads"
            path = "config/" <> allHeads
          getConfig allHeads >>= \case
            Nothing -> text $ "No config file found in " <> path
            Just bytes -> case T.decodeUtf8' bytes of
              Left ue -> text $ "Couldn't decode " <> path <> " : " <> T.pack (show ue)
              Right heads -> do
                let headUrls = T.lines heads
                forM_ headUrls $ \headUrl -> do
                  ws <- createSocket headUrl
                  displayHead ws
    }

createSocket ::
  ( Aeson.FromJSON b
  , MonadJSM m
  , MonadJSM (Performable m)
  , PostBuild t m
  , TriggerEvent t m
  , PerformEvent t m
  , MonadHold t m
  ) =>
  T.Text ->
  m (RawWebSocket t (Maybe b))
createSocket headUrl = jsonWebSocket @Aeson.Value headUrl $ def

displayHead ::
  ( DomBuilder t m
  , PostBuild t m
  , MonadHold t m
  , MonadFix m
  ) =>
  RawWebSocket t (Maybe Aeson.Value) ->
  m ()
displayHead ws = do
  _receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] $ _webSocket_recv ws
  headId <-
    foldDyn
      ( \v d ->
          case parseHeadId (Aeson.encode v) of
            Nothing -> d
            Just headId -> headId
      )
      mempty
      (_webSocket_recv ws)
  headStatus <-
    foldDyn
      ( \v d ->
          case parseHeadStatus (Aeson.encode v) of
            Nothing -> d
            Just headSt -> headSt
      )
      mempty
      (_webSocket_recv ws)
  _ <-
    elClass "div" "container" $ do
      elClass "table" "table-auto" $ do
        el "tr" $ do
          el "th" $ text "Head ID:"
          el "th" $ text "Head Status:"
        el "tr" $ do
          el "td" $ dynText headId
          el "td" $ dynText headStatus
  return ()

parseHeadId :: ByteString -> Maybe T.Text
parseHeadId v = v ^? key "headId" . _String

parseHeadStatus :: ByteString -> Maybe T.Text
parseHeadStatus v = v ^? key "headStatus" . _String
