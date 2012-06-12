{-# LANGUAGE OverloadedStrings #-}

module Handler.Auth
    ( loginH
    ) where

------------------------------------------------------------------------------
import Snap.Core

import Application

------------------------------------------------------------------------------
-- | Displays the login form. (later)
loginH :: AppHandler ()
loginH = writeText "login form"
