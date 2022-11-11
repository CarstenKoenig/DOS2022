{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib.App.Env
  ( Env (..),
    Has (..),
    DbConnection,
    grab,
  )
where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.Kind (Type)
import Lib.Db.DbConnection (DbConnection)

newtype Env (m :: Type -> Type) = Env
  { dbConnection :: DbConnection
  }

-- | Typklasse um @field@ innerhalb @env@ aufzulösen.
--
-- Typen müssen innherhalb der @env@ eindeutig sein.
-- Gegebennfals sollten @newtype@-Wrapper verwendet werden
-- um Eindeutigkeit herustellen.
--
-- Beispiel
-- @
-- foo = do
--    con <- grab @Connection
-- @
class Has field env where
  obtain :: env -> field

instance Has DbConnection (Env m) where obtain = dbConnection

grab :: forall field env m. (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}