{-# LANGUAGE OverloadedStrings #-}

module Controller.Student
    ( studentHomeHandler
    ) where

------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist (render)

import           Application
import           Util.Auth (withAuth)


------------------------------------------------------------------------------
-- | Renders the student area.
studentHomeHandler :: AppHandler ()
studentHomeHandler = withStudent $ render "student/index"


------------------------------------------------------------------------------
-- | Allow only students the access to the handler.
withStudent :: AppHandler () -> AppHandler ()
withStudent = withAuth $ Role "Student"
