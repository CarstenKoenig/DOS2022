{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson (FromJSON (parseJSON), defaultOptions)
import Data.Aeson.Types (genericParseJSON)
import Data.Int (Int64)
import Data.Text
import Lib.Core.Task (Task)
import Servant (Capture, DeleteNoContent, Get, JSON, NamedRoutes, Patch, Post, QueryParam, ReqBody, type (:>))
import Servant.API.Generic (Generic, (:-))

type API = NamedRoutes TodoListAPI

data TodoListAPI mode = TodoListAPI
  { echo :: mode :- "echo" :> QueryParam "text" String :> Get '[JSON] String,
    getAllTasks :: mode :- "task" :> Get '[JSON] [Task],
    insertTask :: mode :- "task" :> ReqBody '[JSON] NewTask :> Post '[JSON] Task,
    getTask :: mode :- "task" :> Capture "taskid" Int64 :> Get '[JSON] Task,
    deleteTask :: mode :- "task" :> Capture "taskid" Int64 :> DeleteNoContent,
    modifyTask :: mode :- "task" :> Capture "taskid" Int64 :> ReqBody '[JSON] ModifyTask :> Patch '[JSON] Task
  }
  deriving (Generic)

newtype NewTask = NewTask {description :: Text}
  deriving stock (Generic, Show, Eq)

instance FromJSON NewTask where
  parseJSON = genericParseJSON defaultOptions

data ModifyTask = ModifyTask {newDescription :: Text, newCompleted :: Bool}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON)
