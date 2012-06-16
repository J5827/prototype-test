{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | Auth controller module.
module Controller.Auth
    ( loginHandler
    , logoutHandler
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding    as T
import           Data.Text             (Text)

import           Snap.Core           (redirect)
import           Snap.Snaplet        (with)
import           Snap.Snaplet.Auth   (Password(..), loginByUsername, logout)
import           Text.Digestive.Snap (runForm)

import           Application (AppHandler, auth)
import           Form.Login  (LoginData(..), loginForm)
import           Util.Form   (showForm)


------------------------------------------------------------------------------
-- | Displays the login form. (later)
loginHandler :: AppHandler ()
loginHandler = do
    (view, loginData) <- runForm "form" loginForm
    maybe (showForm "login" view) performLogin loginData


------------------------------------------------------------------------------
-- | Set the user authenticated to the session after he the login data has
-- alreay been approved by the login form.
performLogin :: LoginData -> AppHandler ()
performLogin loginData = do
    with auth . loginByUsername username password $ loginRemember loginData
    redirect "/"
  where
    username = T.encodeUtf8 $ loginUsername loginData
    password = ClearText . T.encodeUtf8 $ loginPassword loginData


------------------------------------------------------------------------------
-- | Handler to perform the logout action.
logoutHandler :: AppHandler ()
logoutHandler = do
    with auth logout
    redirect "/"
