{-# LANGUAGE FlexibleContexts #-}

module Lib.Db.Schema
  ( prepareDbTest,
    prepareDbProd,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.String (IsString (fromString))
import Lib.Db.Functions (WithDb, executeRaw)

-- | Initialisiert die Task-Tabelle:
-- 1. Erstellt die Tabelle
prepareDbProd :: (WithDb env m) => m ()
prepareDbProd = setupDb

-- | (Re-)Initialisiert die Task-Tabelle:
-- 1. Löscht die vorhandene Tabelle
-- 2. Erstellt die Tabelle
-- 3. Fügt ein Test-Datum ein
prepareDbTest :: (WithDb env m) => m ()
prepareDbTest = teardownDb >> setupDb >> seedDb

-- | Löscht die Tabellen mit Hilfe der @Sql/drop.sqlQ Datei
teardownDb :: (WithDb env m) => m ()
teardownDb = executeFile "Sql/drop.sql"

-- | Erstellt die Tabellen mit Hilfe der @Sql/schema.sql@ Datei.
setupDb :: (WithDb env m) => m ()
setupDb = executeFile "Sql/schema.sql"

-- | Testdaten aus @Sql/seed.sql@ einfügen
seedDb :: (WithDb env m) => m ()
seedDb = executeFile "Sql/seed.sql"

executeFile :: (WithDb env m) => FilePath -> m ()
executeFile path = do
  sqlStatements <- liftIO $ readFile path
  _ <- executeRaw (fromString sqlStatements)
  pure ()