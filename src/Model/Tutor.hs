------------------------------------------------------------------------------
-- | The module containing the tutor model.
module Model.Tutor where

------------------------------------------------------------------------------
import           Model.Course
import           Model.Task


------------------------------------------------------------------------------
-- | The tutor is the head of everything. Joke apart, but this data type is
-- the starting point of the whole task and course organization process. A
-- tutor is a user type that can create (and later modify) courses and set
-- course parameter. He can also create (configure) new tasks via the autotool
-- backend server and store them. Those tasks can then be attached to one or
-- more courses (see course documentation for further explaination).
--
-- How tutors can share tasks with each other and how multiple tutors can be
-- responsible for one course (this is possible without further conflicts)
-- might be specified later.
data Tutor = Tutor
    { tutorUserId   :: Int        -- ^ the userId from the auth backend
    , tutorFirstname :: String
    , tutorLastname  :: String
    , courses        :: [Course]  -- ^ courses the tutor is supervising
    , tasks          :: [Task]    -- ^ tasks that are owned by the tutor
    } deriving (Show) -- FIXME this is just for testing purposes
