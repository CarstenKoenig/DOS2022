{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.Effects.Tasks
  ( MonadTasks (..),
    WithTasks,
  )
where

import Data.Text (Text)
import Lib.App (App (..))
import Lib.Core.Task (Id, Task)
import Lib.Db.Functions (WithDb)
import qualified Lib.Db.Task as DbTask

-- | Beschreibt eine Monade, die @Task@s aus einer Datenbank verwalten kann
class Monad m => MonadTasks m where
  getAllTasks :: m [Task]
  getTask :: Id -> m (Maybe Task)
  insertNew :: Text -> m Task
  modifyTask :: Id -> Text -> Bool -> m (Maybe Task)
  deleteTask :: Id -> m ()

instance MonadTasks App where
  getAllTasks = getAllImpl
  getTask = getTaskImpl
  insertNew = insertTaskImpl
  modifyTask = modifyTaskImpl
  deleteTask = deleteTaskImpl

type WithTasks env m = (WithDb env m)

getAllImpl :: WithDb env m => m [Task]
getAllImpl =
  DbTask.getAllTasks

getTaskImpl :: WithDb env m => Id -> m (Maybe Task)
getTaskImpl =
  DbTask.getTask

insertTaskImpl :: WithDb env m => Text -> m Task
insertTaskImpl = DbTask.insertNewTask

modifyTaskImpl :: WithDb env m => Id -> Text -> Bool -> m (Maybe Task)
modifyTaskImpl = DbTask.modifyTask

deleteTaskImpl :: WithDb env m => Id -> m ()
deleteTaskImpl = DbTask.deleteTask