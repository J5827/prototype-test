{-# LANGUAGE OverloadedStrings #-}

module Controller.Course
    ( createCourseHandler
    ) where

------------------------------------------------------------------------------
import qualified Data.Text as T

import           Snap.Snaplet.Heist    (heistLocal, render)
import           Text.Digestive.Snap   (runForm)
import           Text.Templating.Heist (bindString)

import           Application  (AppHandler)
import           Form.Course  (CourseData(..), courseForm)
import           Model.Course (createCourse)
import           Util.Auth    (getUserId, withTutor)
import           Util.Form    (showForm)


------------------------------------------------------------------------------
-- | Render a form to display a course.
createCourseHandler :: AppHandler ()
createCourseHandler = withTutor $ do
    (view, courseData) <- runForm "form" courseForm
    maybe (showForm "course" view) createCourseFromData courseData


------------------------------------------------------------------------------
-- | Create a new course from the entered data.
--
-- TODO Submit a user information message into the session that can then be
-- displayed on the main page instead of displaying a separate page.
createCourseFromData :: CourseData -> AppHandler ()
createCourseFromData (CourseData name capacity) = do
    uid <- getUserId
    success <- createCourse (T.unpack name) (read $ T.unpack capacity) uid
    let template = if success
                     then "messages/course-create-success"
                     else "messages/course-create-failure"
    heistLocal (bindString "courseName" name) $ render template
