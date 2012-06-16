------------------------------------------------------------------------------
-- | The module containing the student model.
module Model.Student where

------------------------------------------------------------------------------
import           Model.Solution

------------------------------------------------------------------------------
-- | A Student is a simple user type that can enrol in courses and then
-- receive tasks that are assigned to this course. He can create n solutions
-- for a task which will be stored in the sytem (the task which the solution
-- belongs to is referenced in the solution data type).
--
-- Later a permission (assessment) system will be implemented that allows to
-- manage the status to either pass a course or to provide the permission to
-- an exam.
--
-- Later also a highscore system might be interesting to motivate the students
-- to submit good (better) solutions.
data Student = Student
    { studentUserId    :: Int     -- ^ the userId from the auth backend
    , studentFirstname :: String
    , studentLastname  :: String
    , solutions        :: [Solution]
    } deriving (Show) -- FIXME this is just for testing purposes
