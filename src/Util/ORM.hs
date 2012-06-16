------------------------------------------------------------------------------
-- | Library for object relationship mapping.
--
-- The following table layout conventions HAVE TO be followed to use this orm
-- system:
--
-- * every table has to have an id (INTEGER) column which is named after the
--   table, e.g. table 'user' and id column 'userId'
-- * the column with the foreign keys is named like the foreign table plus an
--   's', e.g. the foreign column 'messages' refers to the table 'message'
-- * the foreign keys are given as list of numbers as a string value, e.g.
--   '1,2,3'
--
module Util.ORM
    ( addRelation
    , createRecord
    , getRelatedRecords
    , getLastInsertRowId
    , updateRecord
    ) where

------------------------------------------------------------------------------
import           Data.List (intersperse)
import           Data.Map ((!))

import           Database.HDBC.Sqlite3
import           Snap.Snaplet.Hdbc


------------------------------------------------------------------------------
-- | Find all related records of a specific record with foreign keys.
getRelatedRecords :: HasHdbc m c s => String -> String -> String -> m [Row]
getRelatedRecords sourceTable targetTable idValue = do
    foreignIds <- selectColumn sourceTable idValue (targetTable ++ "s")
    findAllBy targetTable foreignIds


------------------------------------------------------------------------------
-- | Add a relation by adding an id to a foreign column of a table.
addRelation :: HasHdbc m c s => String -> String -> String -> String -> m Integer
addRelation table idValue column foreignId = do
    keys <- selectColumn table idValue column
    let newKeys = if null keys
                    then foreignId
                    else keys ++ "," ++ foreignId
    updateRecord table idValue column (show newKeys)
  

------------------------------------------------------------------------------
-- | Create a new record in the database. Convert all values to strings before
-- passing it to this functions, so a list that looks like ["\"Foo\"", "9"]
-- would have one string and one integer value.
createRecord :: HasHdbc m c s => String -> [String] -> [String] -> m Integer
createRecord table columns values =
    query' queryString []
  where
    queryString = "INSERT INTO " ++
                    table ++ "(" ++ convert columns ++ ")" ++
                  "VALUES(" ++ convert values ++ ")"
    convert = concat . intersperse ", "



------------------------------------------------------------------------------
-- | Update a column of a record.
updateRecord :: HasHdbc m c s
             => String
             -> String
             -> String
             -> String
             -> m Integer
updateRecord table idValue column newValue = do
    query' queryString []
  where
    queryString = "UPDATE " ++ table ++
                  " SET " ++ column ++ " = " ++ newValue ++
                  " WHERE " ++ table ++ "Id = " ++ idValue


------------------------------------------------------------------------------
-- | Get the last inserted row id from a specific table.
getLastInsertRowId :: HasHdbc m c s => String -> m Integer
getLastInsertRowId table = do
    rows <- query queryString []
    if null rows
      then return 0
      else return . fromSql $ head rows ! "seq"
  where
    queryString = "SELECT seq FROM sqlite_sequence " ++
                  "WHERE name=" ++ show table


------------------------------------------------------------------------------
-- | Query the database to select all records from a list of ids.
findAllBy :: HasHdbc m c s => String -> String -> m [Row]
findAllBy table idValues = --return []
    query queryString []
  where
    queryString = "SELECT * FROM " ++
                    table ++
                  " WHERE " ++
                    table ++ "Id IN (" ++ idValues ++ ")"


------------------------------------------------------------------------------
-- | Select the target column from the record of the table that had the passed
-- id value in the id column.
selectColumn :: HasHdbc m c s => String -> String -> String -> m String
selectColumn table idValue targetColumn =  do
    rows <- query queryString []
    if null rows
      then return ""
      else return . fromSql $ head rows ! targetColumn
  where
    queryString = "SELECT * FROM " ++
                    table ++
                  " WHERE " ++
                    table ++ "Id = " ++ idValue
