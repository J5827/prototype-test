------------------------------------------------------------------------------
-- | The module containing the solution model.
module Model.Solution where

------------------------------------------------------------------------------
import           Model.Task

------------------------------------------------------------------------------
-- | A solution for a tasks that has been created by a student. It belongs to
-- one specific task instance as they can be different depending on the seed.
--
-- Main part of the assessment system but most important is actually just the
-- score and maybe a date later. Might be used somehow for highscoring later.
--
-- Together with the student (id) the solution belongs to and the task it has
-- stored a 're-assessment' can be invoked at any time provided the backend
-- server does not change. If it could change, it might be advised to store
-- the whole evaluation as well, but this is left out for now as it is very
-- space expensive.
--
-- The score is stored anyway as it is important for the student's assessment
-- and eventually the highscore later.
data Solution = Solution
    { solutionId :: Int     -- ^ an internal identification number
    , task       :: Task  --
    , score      :: Int
    } deriving (Show) -- FIXME
