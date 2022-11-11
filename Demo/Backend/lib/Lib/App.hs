module Lib.App
  ( App (..),
    runApp,
    Env (..),
    AppEnv,
    Has (..),
    grab,
  )
where

import Lib.App.Env (Env (..), Has (..), grab)
import Lib.App.Monad (App (..), AppEnv, runApp)