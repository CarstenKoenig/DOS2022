module Api.Tasks where

import Prelude

import Affjax.RequestBody as RB
import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (printError)
import Affjax.Web as AJ
import Data.Argonaut (decodeJson, encodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Models.Task (ModifyTask, Task, NewTask)
import Models.Task as Task

getAll :: forall m. MonadAff m => m (Either String (Array Task))
getAll = do
  response <- liftAff $ AJ.get (ResponseFormat.json) $ "http://localhost:8081/task"
  pure do
    { body: json } <- lmap printError response
    lmap printJsonDecodeError $ decodeJson json

newTask :: forall m. MonadAff m => NewTask -> m (Either String Task)
newTask new = do
  response <- liftAff $ AJ.post
    (ResponseFormat.json)
    "http://localhost:8081/task/"
    (Just $ RB.json $ encodeJson new)
  pure do
    { body: json } <- lmap printError response
    lmap printJsonDecodeError $ decodeJson json

patchTask :: forall m. MonadAff m => Task.Id -> ModifyTask -> m (Either String Task)
patchTask taskId modified = do
  response <- liftAff $ AJ.patch
    (ResponseFormat.json)
    ("http://localhost:8081/task/" <> show taskId)
    (RB.json $ encodeJson modified)
  pure do
    { body: json } <- lmap printError response
    lmap printJsonDecodeError $ decodeJson json

deleteTask :: forall m. MonadAff m => Task.Id -> m (Either String Unit)
deleteTask taskId = do
  response <- liftAff $ AJ.delete_ ("http://localhost:8081/task/" <> show taskId)
  pure $ lmap printError response