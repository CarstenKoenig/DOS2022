{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib.Db.Task
  ( getAllTasks,
    getTask,
    insertNewTask,
    deleteTask,
    modifyTask,
  )
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.SQLite.Simple (NamedParam (..))
import Database.SQLite.Simple.QQ (sql)
import Lib.Core.Task (Id (..), Task (Task))
import Lib.Db.Functions (WithDb, execute, executeNamed, execute_, query, query_)

getAllTasks :: (WithDb env m) => m [Task]
getAllTasks =
  query_
    [sql|
         SELECT id, description, completed
         FROM tasks
    |]

getTask :: (WithDb env m) => Id -> m (Maybe Task)
getTask (Id taskId) =
  listToMaybe
    <$> query
      [sql|
         SELECT id, description, completed
         FROM tasks
         WHERE id = ?
      |]
      [taskId]

insertNewTask :: (WithDb env m) => Text -> m Task
insertNewTask description = do
  newId <-
    Id
      <$> execute
        [sql|
         INSERT INTO tasks (description, completed, created_at)
         VALUES (?, 0, datetime('now'));
      |]
        [description]
  pure $ Task newId description False

deleteTask :: (WithDb env m) => Id -> m ()
deleteTask (Id taskId) =
  execute_
    [sql|
         DELETE
         FROM tasks
         WHERE id = ?
      |]
    [taskId]

modifyTask :: (WithDb env m) => Id -> Text -> Bool -> m (Maybe Task)
modifyTask tId@(Id taskId) newDescription newCompleted = do
  _ <-
    executeNamed
      [sql|
         UPDATE tasks
         SET description = :desc, completed = :comp 
         WHERE id = :id;
      |]
      [":id" := taskId, ":desc" := newDescription, ":comp" := newCompleted]
  getTask tId
