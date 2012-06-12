{-# LANGUAGE OverloadedStrings #-}

module Controller.Index
    ( indexHandler
    ) where

------------------------------------------------------------------------------
import Snap.Core
import Snap.Snaplet.Auth

import Application
import Controller.Auth


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
indexHandler :: AppHandler ()
indexHandler = ifTop $ requireUser auth loginHandler roleSwitchHandler


------------------------------------------------------------------------------
-- | Route to user specific handler depending on role - tutor or student.
roleSwitchHandler :: AppHandler ()
roleSwitchHandler = writeText "logged in"
