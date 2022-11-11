{-# LANGUAGE RecordWildCards #-}

module Lib.Db.DbConnection
  ( DbConnection,
    create,
    withSqliteConnection,
  )
where

import Database.SQLite.Simple (Connection, withConnection)

newtype DbConnection = DbConnection {conString :: String}

-- | erstellt eine @DbConnection@ aus einem SQLite-ConnectionString
create :: String -> DbConnection
create = DbConnection

-- | erstellt eine neue Connection, führt die übergebene Anfrage darüber aus
-- und kümmert sich darum, dass die Resourcen aufgeräumt werden
withSqliteConnection :: DbConnection -> (Connection -> IO a) -> IO a
withSqliteConnection (DbConnection {..}) =
  withConnection conString