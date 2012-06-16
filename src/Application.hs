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

import           Database.HDBC.Sqlite3 (Connection)
import           Snap                  (get)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Hdbc
import           Snap.Snaplet.Session


------------------------------------------------------------------------------
-- | Application snaplet containing other snaplets as sub components.
data App = App
    { _heist :: Snaplet (Heist App)        -- ^ heist snaplet
    , _sess  :: Snaplet SessionManager     -- ^ session snaplet
    , _auth  :: Snaplet (AuthManager App)  -- ^ authentication snaplet
    , _db    :: Snaplet (HdbcSnaplet Connection IO)  -- ^ database snaplet
    }

makeLens ''App


------------------------------------------------------------------------------
-- | App Heist instance.
instance HasHeist App where
    heistLens = subSnaplet heist


------------------------------------------------------------------------------
-- | AppHandler Hdbc instance.
instance HasHdbc AppHandler Connection IO where
    getHdbcState = with db get

------------------------------------------------------------------------------
-- | Synonym for handler.
type AppHandler = Handler App App


