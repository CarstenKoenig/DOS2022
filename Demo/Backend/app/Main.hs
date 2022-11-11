module Main where

import App (app)
import Lib.App (Env (Env), runApp)
import qualified Lib.Db as DB
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  let env = Env (DB.create "db.sqlite")
  -- Datenbank initialisieren / seeden
  runApp env DB.prepareDbProd
  putStrLn "Server starting on http://localhost:8081"
  run 8081 (app env)
