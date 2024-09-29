{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}

module Frontend where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Obelisk.Frontend
import Data.Foldable (forM_)
import Obelisk.Route
import Obelisk.Generated.Static
import Language.Javascript.JSaddle (MonadJSM, jsg, js, js1, liftJSM, fromJSValUnchecked)

import Obelisk.Configs
import Reflex.Dom.Core

import Common.Api
import Common.Route
default (T.Text)


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Hydra Head Registry"
      elAttr "meta" ("name"=:"viewport" <> "content"=:"width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no") blank
      elAttr "script" ("type" =: "application/javascript" <> "src" =: $(static "lib.js")) blank
      elAttr "link" ("href" =: $(static "main.css") <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Hydra Head Registry"
      el "p" $ text $ T.pack commonStuff
      prerender_ (text "Loading Live Documentation") $ do
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
            let wsurl = "ws://" <> headUrl  

            rec
              t <- inputElement def
              b <- button "Send"
              text $ "Sending to " <> wsurl
              let newMessage = fmap ((:[]) . T.encodeUtf8) $ tag (current $ value t) $ leftmost [b, keypress Enter t]
            ws <- webSocket wsurl $ def & webSocketConfig_send .~ newMessage 
            receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] $ _webSocket_recv ws
            el "p" $ text "Responses from :"
            _ <- el "ul" $ simpleList receivedMessages $ \m -> el "li" $ dynText =<< mapDynM (pure . T.decodeUtf8) m

            return ()
  }
