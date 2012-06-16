------------------------------------------------------------------------------
-- | The module containing the task model.
--
-- The discussion about the use of existing primitive data types (String, Int)
-- vs. specific ones (Name, Config, etc.) may start here.
module Model.Task where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import           Data.Map ((!))

import           Snap.Snaplet.Hdbc

import           Types (ScoringOrder(..))
import           Util.ORM


------------------------------------------------------------------------------
-- | A task is actually a already configured task by a tutor. Despite the name
-- collision with the autotool backend server protocol (autOlat) this data
-- type is just named 'Task' for simplicity reasons.
--
-- A task already contains a set configuration and a signature and belongs to
-- exactly one tutor who configured the task. He may or may not assign this
-- task to one or multiple courses where it can then be seen and solved by
-- students.
--
-- Note that such a task is first 'instanciated' (see autOlat) when the
-- student opens it as the seed is depending on each student individually.
--
-- A task can have one or more solutions but this relationship is stored in
-- the solution data type.
--
-- It is also important to store the scoring order for a correct high score
-- ranking later. More fields might also be added later like 'createdAt' etc.
-- and especially 'startDate' and 'dueDate' for the permission system.
data Task = Task
    { taskId       :: Int           -- ^ an internal identification number
    , taskName     :: String        -- ^ the task name from the autotool
    , taskConfig   :: String        -- ^ the task configuration as verified
    , signature    :: String        -- ^ the hash key given by the server
    , scoringOrder :: ScoringOrder
    } deriving (Show) -- FIXME


------------------------------------------------------------------------------
-- | Query the database to load all tasks that have a reference to the tutor
-- with the passed id.
getTutorTasks :: HasHdbc m c s => String -> m [Task]
getTutorTasks uid = do
    records <- getRelatedRecords "tutor" "task" uid
    return $ map toTask records


------------------------------------------------------------------------------
-- | Convert a task record that is read from the db into the corresponding
-- haskell data type.
toTask :: Row -> Task
toTask row = Task
               (read . BS.unpack . fromSql $ row ! "taskId")
               (fromSql $ row ! "taskName")
               ""
               ""
               None

