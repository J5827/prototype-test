{-# LANGUAGE OverloadedStrings #-}

module Controller.Student
    ( studentHomeHandler
    ) where

------------------------------------------------------------------------------
import           Snap.Core
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist (render)

import           Application
import           Util.Auth (withStudent)


------------------------------------------------------------------------------
-- | Renders the student area.
studentHomeHandler :: AppHandler ()
studentHomeHandler = withStudent $ render "student/index"
