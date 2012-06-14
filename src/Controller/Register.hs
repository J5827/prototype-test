{-# LANGUAGE OverloadedStrings #-}

module Controller.Register
    ( registrationHandler
    ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BS

import           Snap
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Text.Digestive.View
import           Text.Templating.Heist

import           Application
import           Form.Login
import           Form.Registration
import           Util.Form
import           Util.PasswordGenerator


------------------------------------------------------------------------------
-- | Handler to display and process the registration form.
registrationHandler :: AppHandler ()
registrationHandler = do
    (view, registrationData) <- runForm "form" registrationForm
    maybe (showForm "registration" view) performRegistration registrationData


------------------------------------------------------------------------------
-- | Create a new user from the entered registration data and store it in a
-- json file. Note: currently just the student id and the password are saved.
performRegistration :: RegistrationData -> AppHandler ()
performRegistration regData = do
    password <- liftIO createRandomPassword
    user <- with auth $ createUser userId password
    with auth $ saveUser user { userRoles = [Role "Student"] }
    heistLocal (bindStrings $ userData password) $ render "registration-done"
    -- writeText $ T.pack $ show user'
  where 
    userId = regUsername regData
    userData password = [
        ("name",     regFirstname regData)
      , ("password", T.decodeUtf8 password)
      ]
