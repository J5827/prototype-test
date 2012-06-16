{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Course form powered by digestive functors.
module Form.Course
  ( CourseData(..)
  , courseForm
  ) where

------------------------------------------------------------------------------
import           Data.Text (Text)

import           Snap           ((<$>), (<*>))
import           Text.Digestive (Form, (.:), check, text)

import           Application (AppHandler)
import           Util.Form   (notEmpty)


------------------------------------------------------------------------------
-- | Data type for digestive course form.
data CourseData = CourseData
  { courseName     :: Text
  , courseCapacity :: Text
  } deriving (Show)


------------------------------------------------------------------------------
-- | Digestive course form.
courseForm :: Form Text AppHandler CourseData
courseForm = CourseData
      <$> "coursename" .: check coursenameEmptyMsg notEmpty (text Nothing)
      <*> "capacity"   .: text Nothing


------------------------------------------------------------------------------
-- | Error message.
coursenameEmptyMsg :: Text
coursenameEmptyMsg = "bitte Kursnamen eingeben" 
