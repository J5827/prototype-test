{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Auth helper library to provide functions such as authentication guards or
-- to get the id from the current logged in user.
module Util.Auth
    ( getUserId
    , withTutor
    , withStudent
    ) where

------------------------------------------------------------------------------
import           Data.Maybe (fromJust, isJust)
import qualified Data.Text  as T

import           Snap              ((<$>))
import           Snap.Core
import           Snap.Snaplet      (with)
import           Snap.Snaplet.Auth

import           Application


------------------------------------------------------------------------------
-- | Unwraps the userId record field of the currently logged in user. Uses the
-- unsafe fromJust, so it need to be called after a user check (e.g. withAuth)
-- has already performed.
getUserId :: AppHandler String
getUserId = do
    user <- fromJust <$> with auth currentUser
    return . T.unpack . unUid . fromJust $ userId user

------------------------------------------------------------------------------
-- | Allow only tutors the access to the handler.
withTutor :: AppHandler () -> AppHandler ()
withTutor = withAuth $ Role "Tutor"


------------------------------------------------------------------------------
-- | Allow only students the access to the handler.
withStudent :: AppHandler () -> AppHandler ()
withStudent = withAuth $ Role "Student"


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
