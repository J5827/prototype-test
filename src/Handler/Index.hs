{-# LANGUAGE OverloadedStrings #-}

module Handler.Index
    ( indexH
    ) where

------------------------------------------------------------------------------
import Snap.Core
import Snap.Snaplet.Auth

import Application
import Handler.Auth


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
indexH :: AppHandler ()
indexH = ifTop $ requireUser auth loginH roleSwitchH


------------------------------------------------------------------------------
-- | Route to user specific handler depending on role - tutor or student.
roleSwitchH :: AppHandler ()
roleSwitchH = writeText "logged in"
