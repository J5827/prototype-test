{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
--   handler monad.
--
module Application where

------------------------------------------------------------------------------
import           Data.Lens.Template (makeLens)

import           Database.HDBC.Sqlite3
import           Snap (get)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Snaplet (HdbcSnaplet Connection IO)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasHdbc AppHandler Connection IO where
    getHdbcState = with db get

------------------------------------------------------------------------------
type AppHandler = Handler App App


