{-# LANGUAGE OverloadedStrings #-}

module Controller.Auth
    ( loginHandler
    ) where

------------------------------------------------------------------------------
import Snap.Core

import Application

------------------------------------------------------------------------------
-- | Displays the login form. (later)
loginHandler :: AppHandler ()
loginHandler = writeText "login form"
