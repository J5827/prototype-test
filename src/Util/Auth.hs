{-# LANGUAGE OverloadedStrings #-}

module Util.Auth
    ( withAuth
    ) where

------------------------------------------------------------------------------
import           Data.Maybe (fromJust, isJust)

import           Snap.Core
import           Snap.Snaplet (with)
import           Snap.Snaplet.Auth

import           Application


------------------------------------------------------------------------------
-- | Allow only user with a specific role the access to the handler.
--
-- If the user is not authenticated or has the wrong role redirect to the
-- index handler. Allow only user with a specific role the access to the
-- handler.
withAuth :: Role -> AppHandler () -> AppHandler ()
withAuth role targetHandler = do
    user <- with auth currentUser
    if isJust user && elem role (userRoles . fromJust $ user)
      then targetHandler
      else redirect "/"
