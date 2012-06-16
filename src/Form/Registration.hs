{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Registration form powered by digestive functors.
module Form.Registration
  ( RegistrationData(..)
  , registrationForm
  ) where

------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T

import           Snap
import           Snap.Snaplet.Auth
import           Text.Digestive
import qualified Text.Email.Validate as E

import           Application
import           Util.Form


------------------------------------------------------------------------------
-- | Data type for digestive login form.
data RegistrationData = RegistrationData
  { regUsername  :: Text
  , regFirstname :: Text
  , regLastname  :: Text
  , regEmail     :: Text
  } deriving (Show)


------------------------------------------------------------------------------
-- | Login form for a user.
registrationForm :: Form Text AppHandler RegistrationData
registrationForm =
    checkM usernameInUseMsg validUsername $ RegistrationData
      <$> "username"  .: check usernameEmptyMsg  notEmpty (text Nothing)
      <*> "firstname" .: check firstnameEmptyMsg notEmpty (text Nothing)
      <*> "lastname"  .: check lastnameEmptyMsg  notEmpty (text Nothing)
      <*> "email"     .: check emailInvalidMsg validEmail (text Nothing)



------------------------------------------------------------------------------
-- | Checks with the auth backend if the username is already in use.
validUsername :: RegistrationData -> AppHandler Bool
validUsername = liftM not . with auth . usernameExists . regUsername


-----------------------------------------------------------------------------
-- | Function to verify the email via Text.Email.Validate.
--
-- TODO: check if email is already in use also, as one email should not be
-- use with two different logins, should it? if so where do you send the
-- password to? ;) but this can just be done after a persistent storage of the
-- user details as currently just username and password is stored
-- persistently.
validEmail :: Text -> Bool
validEmail = E.isValid . T.unpack


-----------------------------------------------------------------------------
-- | Error message.
usernameInUseMsg :: Text
usernameInUseMsg  = "Matrikelnummer bereits registriert"

-----------------------------------------------------------------------------
-- | Error message.
usernameEmptyMsg :: Text
usernameEmptyMsg = "bitte Matrikelnummer eingeben" 

-----------------------------------------------------------------------------
-- | Error message.
firstnameEmptyMsg :: Text
firstnameEmptyMsg = "bitte Vornamen eingeben" 

-----------------------------------------------------------------------------
-- | Error message.
lastnameEmptyMsg :: Text
lastnameEmptyMsg = "bitte Nachnamen eingeben" 

-----------------------------------------------------------------------------
-- | Error message.
emailInvalidMsg :: Text
emailInvalidMsg = "keine gültige E-Mail-Adresse"
