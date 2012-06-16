{-# LANGUAGE OverloadedStrings #-}

module Controller.Tutor
    ( tutorHomeHandler
    ) where

------------------------------------------------------------------------------
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromJust)
import           Data.Text             (Text)
import qualified Data.Text             as T

import           Snap                  ((<$>))
import           Snap.Snaplet.Heist    (heistLocal,render)
import           Text.Templating.Heist (Splice, bindSplices, mapSplices,
                                        runChildrenWithText)

import           Application  (AppHandler)
import           Util.Auth    (getUserId, withTutor)
import           Model.Course (Course(..), getTutorCourses)
import           Model.Task   (Task(..), getTutorTasks)


------------------------------------------------------------------------------
-- | Renders the course list and the task list into the tutors area.
tutorHomeHandler :: AppHandler ()
tutorHomeHandler = withTutor $ do
    uid     <- getUserId
    courses <- getTutorCourses uid
    tasks   <- getTutorTasks   uid
    let splices = tutorSplices courses tasks
    heistLocal (bindSplices splices) $ render "tutor/index"
  where
    tutorSplices courses tasks =
      [ ("courseList", mapSplices renderCourse courses)
      , ("taskList",   mapSplices renderTask   tasks)
      ]


------------------------------------------------------------------------------
-- | Splice to render one course into a HTML list element.
renderCourse :: Course -> Splice AppHandler
renderCourse course =
    runChildrenWithText [ ("courseId",   T.pack . show $ courseId course)
                        , ("courseName", T.pack $ courseName course)
                        ]


------------------------------------------------------------------------------
-- | Splice to render one task into a HTML list element.
renderTask :: Task -> Splice AppHandler
renderTask task =
    runChildrenWithText [ ("taskId",   T.pack . show $ taskId task)
                        , ("taskName", T.pack $ taskName task)
                        ]
