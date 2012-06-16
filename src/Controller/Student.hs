{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Student controller to handle student functionalities.
module Controller.Student
    ( studentHomeHandler
    ) where

------------------------------------------------------------------------------
import           Snap.Snaplet.Heist (render)

import           Application (AppHandler)
import           Util.Auth   (withStudent)


------------------------------------------------------------------------------
-- | Renders the student area.
studentHomeHandler :: AppHandler ()
studentHomeHandler = withStudent $ render "student/index"
