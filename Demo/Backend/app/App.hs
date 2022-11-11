{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module App
  ( app,
  )
where

import Api (API, ModifyTask (..), NewTask (..), TodoListAPI (..))
import Control.Monad.Except (ExceptT, MonadTrans (lift), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Lib.App (App, AppEnv, runApp)
import Lib.Core.Task (Id (..), Task)
import qualified Lib.Effects.Tasks as Tasks
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..), cors)
import Servant (Handler, HasServer (ServerT), NoContent (..), Proxy (..), Server, ServerError (..))
import qualified Servant
import Servant.Server (Application, serve)

-- | WAI-Application für die Backend-API
app :: AppEnv -> Application
app env =
  cors (Just . customPolicy) $
    serve (Proxy @API) (server env)
  where
    customPolicy _ =
      CorsResourcePolicy
        { corsOrigins = Nothing,
          corsMethods = ["OPTIONS", "GET", "PUT", "POST", "PATCH", "DELETE"],
          corsRequestHeaders = ["Authorization", "Content-Type"],
          corsExposedHeaders = Nothing,
          corsMaxAge = Nothing,
          corsVaryOrigin = False,
          corsRequireOrigin = False,
          corsIgnoreFailures = False
        }

-- | hoisted führt die @App@ Monade in der @AppEnv@-Umgebung als Servant-Server aus
-- damit ServerError werte aus der Implementation heraus geworfen werden können,
-- wird die @App@ Monade noch in `ExceptT ServerError` gewickelt
server :: AppEnv -> Server API
server env =
  Servant.hoistServer (Proxy @API) runImpl apiServer
  where
    runImpl :: ExceptT ServerError App a -> Handler a
    runImpl impl = do
      res <- liftIO $ runApp env (runExceptT impl)
      case res of
        Left err -> Servant.throwError err
        Right a -> pure a

-- | die Handler innerhalb der @App@-Monade
apiServer :: ServerT API (ExceptT ServerError App)
apiServer =
  TodoListAPI
    { echo = \inp -> do
        let txt = fromMaybe "---" inp
        liftIO $ putStrLn $ "[get] /echo - txt=" ++ txt
        pure txt,
      getAllTasks = getAllTasksImpl,
      insertTask = insertTaskImpl,
      getTask = getTaskImpl,
      deleteTask = deleteTaskImpl,
      modifyTask = modifyTaskImpl
    }

getAllTasksImpl :: ExceptT ServerError App [Task]
getAllTasksImpl = do
  liftIO $ putStrLn "[get] /task - Abfrage aller Tasks"
  lift Tasks.getAllTasks

insertTaskImpl :: Api.NewTask -> ExceptT ServerError App Task
insertTaskImpl NewTask {description} = do
  liftIO $ putStrLn $ "[post] /task - neuer Task mit Beschreibung [" ++ unpack description ++ "]"
  lift $ Tasks.insertNew description

getTaskImpl :: Int64 -> ExceptT ServerError App Task
getTaskImpl taskId = do
  liftIO $ putStrLn $ "[get] /task/" ++ show taskId ++ " - Abfrage eines Tasks"
  res <- lift $ Tasks.getTask (Id taskId)
  case res of
    Nothing -> throwError notFound
    Just task -> pure task
  where
    notFound = Servant.err404 {errBody = "Task nicht gefunden"}

deleteTaskImpl :: Int64 -> ExceptT ServerError App NoContent
deleteTaskImpl taskId = do
  liftIO $ putStrLn $ "[delete] /task/" ++ show taskId ++ " - Task wird gelöscht"
  _ <- lift $ Tasks.deleteTask (Id taskId)
  pure NoContent

modifyTaskImpl :: Int64 -> ModifyTask -> ExceptT ServerError App Task
modifyTaskImpl taskId ModifyTask {..} = do
  liftIO $ putStrLn $ "[patch] /task/" ++ show taskId ++ " - Task wird geändert"
  res <- lift $ Tasks.modifyTask (Id taskId) newDescription newCompleted
  case res of
    Nothing -> throwError notFound
    Just task -> pure task
  where
    notFound = Servant.err404 {errBody = "Task nicht gefunden"}
