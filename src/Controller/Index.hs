{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller for the application home page. Switches automatically to a
-- login form in case the user is not authenticated.
module Controller.Index
    ( indexHandler
    ) where

------------------------------------------------------------------------------
import           Data.Maybe (fromJust)

import           Snap.Core         (ifTop, redirect)
import           Snap.Snaplet      (with)
import           Snap.Snaplet.Auth (Role(..), currentUser, requireUser,
                                    userRoles)

import           Application (AppHandler, auth)


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
indexHandler :: AppHandler ()
indexHandler = ifTop $ requireUser auth (redirect "/login") roleSwitchHandler


------------------------------------------------------------------------------
-- | Route to user specific handler depending on role - tutor or student.
roleSwitchHandler :: AppHandler ()
roleSwitchHandler = do
    user <- with auth currentUser
    redirect . mapHandler . userRoles $ fromJust user
  where
    mapHandler roles | Role "Student" `elem` roles = "/student"
                     | Role "Tutor"   `elem` roles = "/tutor"
                     | otherwise                   = "/error"
