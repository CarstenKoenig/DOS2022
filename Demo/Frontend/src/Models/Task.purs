module Models.Task
  ( Id
  , Task
  , NewTask
  , ModifyTask
  , setComplete
  , setDescription
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Tuple (Tuple(..))

newtype Id = Id Int

derive instance eqId :: Eq Id
derive instance ordId :: Ord Id

instance showId :: Show Id where
  show (Id n) = show n

instance encodeId :: EncodeJson Id where
  encodeJson (Id id) = encodeJson id

instance decodeId :: DecodeJson Id where
  decodeJson json = Id <$> decodeJson json

type Task =
  { taskCompleted :: Boolean
  , taskDescription :: String
  , taskId :: Id
  }

setComplete :: Boolean -> Task -> Tuple Task ModifyTask
setComplete newCompleted task =
  Tuple (task { taskCompleted = newCompleted }) { newCompleted, newDescription: task.taskDescription }

setDescription :: String -> Task -> Tuple Task ModifyTask
setDescription newDescription task =
  Tuple (task { taskDescription = newDescription }) { newDescription, newCompleted: task.taskCompleted }

type NewTask =
  { description :: String
  }

type ModifyTask =
  { newDescription :: String
  , newCompleted :: Boolean
  }