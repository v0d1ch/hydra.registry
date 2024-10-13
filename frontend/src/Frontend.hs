{-# LANGUAGE CPP #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Frontend where

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
        elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
        elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
    , _frontend_body = do
        el "h1" $ text "Hydra Head Registry"
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
                  rec t <- inputElement def
                      b <- button "Send"
                      let newMessage = fmap ((: [])) $ tag (current $ value t) $ leftmost [b, keypress Enter t]
                  ws :: RawWebSocket t (Maybe Aeson.Value) <-
                    jsonWebSocket headUrl $
                      def
                        & webSocketConfig_send .~ newMessage
                  displayHead ws
    }

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
      el "p" $ dynText headId
      el "p" $ dynText headStatus
  return ()

parseHeadId :: ByteString -> Maybe T.Text
parseHeadId v = v ^? key "headId" . _String

parseHeadStatus :: ByteString -> Maybe T.Text
parseHeadStatus v = v ^? key "headStatus" . _String
