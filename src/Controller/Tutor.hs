{-# LANGUAGE OverloadedStrings #-}

module Controller.Tutor
    ( tutorHomeHandler
    ) where

------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist (render)

import           Application
import           Util.Auth (withAuth)


------------------------------------------------------------------------------
-- | Renders the tutor area.
tutorHomeHandler :: AppHandler ()
tutorHomeHandler = withTutor $ render "tutor/index"


------------------------------------------------------------------------------
-- | Allow only tutors the access to the handler.
withTutor :: AppHandler () -> AppHandler ()
withTutor = withAuth $ Role "Tutor"
