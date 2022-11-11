{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Core.Task
  ( Task (..),
    Id (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Database.SQLite.Simple (FromRow)
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import GHC.Generics (Generic)

newtype Id = Id Int64
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON)

-- | Eine Aufgabe - repräsentiert Daten in der @todos@ Tabele.
data Task = Task
  { taskId :: !Id,
    taskDescription :: !Text,
    taskCompleted :: !Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromRow)
  deriving (FromJSON, ToJSON)
