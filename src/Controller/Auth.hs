{-# LANGUAGE OverloadedStrings #-}

module Controller.Auth
    ( loginHandler
    , logoutHandler
    ) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T
import           Data.Text (Text)

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Text.Digestive.Heist
import           Text.Digestive.Snap
import           Text.Digestive.View

import           Application
import           Form.Login
import           Util.Form


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
