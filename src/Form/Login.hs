{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Login form powered by digestive functors.
module Form.Login
  ( LoginData(..)
  , loginForm
  ) where

------------------------------------------------------------------------------
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           Snap
import           Snap.Snaplet.Auth
import           Text.Digestive

import           Application
import           Util.Form


------------------------------------------------------------------------------
-- | Data type for digestive login form.
data LoginData = LoginData
  { loginUsername :: Text
  , loginPassword :: Text
  , loginRemember :: Bool
  } deriving (Show)


------------------------------------------------------------------------------
-- | Login form for a user.
loginForm :: Form Text AppHandler LoginData
loginForm =
    checkM invalidLoginMsg validLogin $ LoginData
      <$> "username" .: check usernameEmptyMsg notEmpty (text Nothing)
      <*> "password" .: check passwordEmptyMsg notEmpty (text Nothing)
      <*> "remember" .: bool (Just False)


------------------------------------------------------------------------------
-- | Try to find the user in the user backend store. Then check, if the role
-- is correct and if the password hash matches. Returns True (in the Handler
-- monad) if every test has passed correctly.
validLogin :: LoginData -> AppHandler Bool
validLogin loginData = do
    authMgr  <- with auth get
    authUser <- liftIO . lookupByLogin authMgr $ loginUsername loginData
    return $ maybe False authenticate authUser
  where
    authenticate = isNothing . flip authenticatePassword password
    password = ClearText . T.encodeUtf8 $ loginPassword loginData


------------------------------------------------------------------------------
-- | Error message.
usernameEmptyMsg :: Text
usernameEmptyMsg = "bitte Matrikelnummer eingeben" 


------------------------------------------------------------------------------
-- | Error message.
passwordEmptyMsg :: Text
passwordEmptyMsg = "bitte Passwort eingeben" 


------------------------------------------------------------------------------
-- | Error message.
invalidLoginMsg :: Text
invalidLoginMsg  = "falsches Passwort"
