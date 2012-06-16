{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Controller to handle user registration.
module Controller.Register
    ( registrationHandler
    ) where

------------------------------------------------------------------------------
import qualified Data.Text.Encoding as T

import           Snap                  (liftIO)
import           Snap.Snaplet          (with)
import           Snap.Snaplet.Auth     (AuthUser(..), Role(..), createUser,
                                        saveUser)
import           Snap.Snaplet.Heist    (heistLocal, render)
import           Text.Digestive.Snap   (runForm)
import           Text.Templating.Heist (bindStrings)

import           Application            (AppHandler, auth)
import           Form.Registration      (RegistrationData(..),
                                         registrationForm)
import           Util.Form              (showForm)
import           Util.PasswordGenerator (createRandomPassword)


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
    user <- with auth $ createUser uid password
    with auth $ saveUser user { userRoles = [Role "Student"] }
    heistLocal (bindStrings $ userData password) $ render "registration-done"
    -- writeText $ T.pack $ show user'
  where 
    uid = regUsername regData
    userData password = [
        ("name",     regFirstname regData)
      , ("password", T.decodeUtf8 password)
      ]
