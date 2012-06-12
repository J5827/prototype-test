------------------------------------------------------------------------------
-- | Module to create a random pasword of alphanumeric chars.
module Util.PasswordGenerator
  ( createRandomPassword
  ) where

import Data.ByteString.Char8 hiding (take)
--import Control.Monad.State  -- later! ;)
import System.Random

------------------------------------------------------------------------------
-- | Create a random password.
createRandomPassword :: IO ByteString
createRandomPassword = do
    gen <- newStdGen
    let pw = take 10 $ randomRs ('a', 'z') gen
    return $ pack pw -- "hvgrn3Kj"
