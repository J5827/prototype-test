------------------------------------------------------------------------------
-- | The module containing the course model.
module Model.Course
    ( Course(..)
    , getTutorCourses
    , createCourse
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Map              ((!))

import           Snap.Snaplet.Hdbc (HasHdbc, Row, fromSql)

import           Model.Student (Student)
import           Model.Task    (Task)
import           Util.ORM      (addRelation, createRecord, getLastInsertRowId,
                                getRelatedRecords)


------------------------------------------------------------------------------
-- | A course can be created by a tutor and contains a certain number of
-- enrolled students. The tutor can assign tasks to the course which will then
-- be seen and solved by all enrolled students.
--
-- Theoretically a course can be assigned to multipe tutors, this may be built
-- in later.
--
-- Also a course may later be assigned to a certain semester and school. It
-- may also have number of tasks and a number of required task in order to
-- realize the permission (assessment) system.
data Course = Course
    { courseId       :: Int
    , courseName     :: String
    , courseCapacity :: Int        -- ^ max number of students
    , courseStudents :: [Student]  -- ^ all enroled students 
    , courseTasks    :: [Task]     -- ^ from tutor assigned tasks
    } deriving (Show) -- FIXME remove


------------------------------------------------------------------------------
-- | Query the database to load all courses that have a reference to the tutor
-- with the passed id.
getTutorCourses :: HasHdbc m c s => String -> m [Course]
getTutorCourses uid = do
    records <- getRelatedRecords "tutor" "course" uid
    return $ map toCourse records


------------------------------------------------------------------------------
-- | Convert a course record that is read from the db into the corresponding
-- haskell data type.
toCourse :: Row -> Course
toCourse row = Course
                 (read . BS.unpack . fromSql $ row ! "courseId")
                 (fromSql                    $ row ! "courseName")
                 (read . BS.unpack . fromSql $ row ! "capacity")
                 [] -- FIXME add student list
                 [] -- FIXME add task list


------------------------------------------------------------------------------
-- | Create a new course in the db via course name and capacity (students and
-- tasks are not assigned initially).
--
-- TODO Add better error handling, change return type to Either String Course.
-- TODO This code should run in a maybe or either monad.
-- TODO The getLastInsertRowId is highly error prone when transactions happen
-- at the same time, but for the prototype it should be okay.
createCourse :: HasHdbc m c s => String -> Int -> String -> m Bool
createCourse name capacity uid = do
    result1 <- createRecord "course" ["courseName", "capacity"]
                                     [show name, show capacity]
    if result1 > 0
      then do cid     <- getLastInsertRowId "course"
              result2 <- addRelation "tutor" uid "courses" (show cid)
              return (result2 > 0)
      else return False
