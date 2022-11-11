{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Lib.Db.Functions
  ( WithDb,

    -- * Sql functions
    query,
    query_,
    queryNamed,
    execute,
    execute_,
    executeRaw,
    executeNamed,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader)
import Data.Int (Int64)
import Database.SQLite.Simple (FromRow, NamedParam, ToRow)
import qualified Database.SQLite.Simple as Sql
import Lib.App.Env (DbConnection, Has, grab)
import qualified Lib.Db.DbConnection as DbConnection

-- | Constraint für Actions, die auf die Datenbank zugreifen wollen
type WithDb env m = (MonadReader env m, Has DbConnection env, MonadIO m)

withDb :: WithDb env m => (Sql.Connection -> IO b) -> m b
withDb use = do
  dbCon <- grab @DbConnection
  liftIO $ DbConnection.withSqliteConnection dbCon use
{-# INLINE withDb #-}

query :: forall res args env m. (WithDb env m, ToRow args, FromRow res) => Sql.Query -> args -> m [res]
query q args = withDb $ \conn -> Sql.query conn q args
{-# INLINE query #-}

query_ :: forall res env m. (WithDb env m, FromRow res) => Sql.Query -> m [res]
query_ q = withDb $ \conn -> Sql.query_ conn q
{-# INLINE query_ #-}

queryNamed :: (WithDb env m, FromRow res) => Sql.Query -> [NamedParam] -> m [res]
queryNamed q params = withDb (\conn -> Sql.queryNamed conn q params)
{-# INLINE queryNamed #-}

executeRaw :: (WithDb env m) => Sql.Query -> m Int64
executeRaw q = withDb $ \conn -> do
  Sql.execute_ conn q
  Sql.lastInsertRowId conn
{-# INLINE executeRaw #-}

execute :: forall args env m. (WithDb env m, ToRow args) => Sql.Query -> args -> m Int64
execute q args = withDb $ \conn -> do
  Sql.execute conn q args
  Sql.lastInsertRowId conn
{-# INLINE execute #-}

execute_ :: forall args env m. (WithDb env m, ToRow args) => Sql.Query -> args -> m ()
execute_ q args = withDb $ \conn -> do
  Sql.execute conn q args
{-# INLINE execute_ #-}

executeNamed :: (WithDb env m) => Sql.Query -> [NamedParam] -> m Int
executeNamed q params =
  withDb
    ( \conn -> do
        Sql.executeNamed conn q params
        Sql.changes conn
    )
{-# INLINE executeNamed #-}
