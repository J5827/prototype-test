{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
--   site. The 'app' function is the initializer that combines everything
--   together and is exported by this module.
--
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Data.ByteString (ByteString)

import           Database.HDBC.Sqlite3
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
-- import           Snap.Snaplet.Auth.Backends.Hdbc
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import           Application
import           Controller.Auth
import           Controller.Index
import           Controller.Register
import           Controller.Student
import           Controller.Tutor


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",         indexHandler)
         , ("/login",    loginHandler)
         , ("/logout",   logoutHandler)
         , ("/register", registrationHandler)
         , ("/student",  studentHomeHandler)
         , ("/tutor",    tutorHomeHandler)
         
           -- tutorial
         , ("/some/:num", someNumHandler)

         , ("",          serveDirectory "resources")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "a snap web front end for the autotool" Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess sessionInit
    a <- nestSnaplet "auth" auth jsonAuthInit
    d <- nestSnaplet "hdbc" db $ hdbcInit sqli
    addRoutes routes
    return $ App h s a d
  where
    sessionInit  = initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    jsonAuthInit = initJsonFileAuthManager defAuthSettings sess "users.json"
    -- hdbcAuthInit = initHdbcAuthManager defAuthSettings sess sqli defAuthTable
                                       -- defQueries
    sqli         = connectSqlite3 "resources/client.db"
