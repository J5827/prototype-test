------------------------------------------------------------------------------
-- | Basic data types for the autotool client. Some are taken over from the
-- autotool but some are amended versions of them or new ones.
module Types
  ( ScoringOrder(..)
  ) where

data ScoringOrder = Increasing
                  | None
                  | Decreasing
                  deriving (Show) -- FIXME
