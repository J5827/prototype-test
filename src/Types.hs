------------------------------------------------------------------------------
-- | Basic data types for the autotool client. Some are taken over from the
-- autotool but some are amended versions of them or new ones.
module Types
  ( ScoringOrder(..)
  ) where


------------------------------------------------------------------------------
-- | Data type to represent the scoring order of tasks in a highscore list.
-- Increasing means a lower score is better, decreasing a higher score is
-- better.
data ScoringOrder = Increasing
                  | None
                  | Decreasing
                  deriving (Show) -- FIXME
