{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.App.Monad
  ( -- * Application monad
    App (..),
    AppEnv,
    runApp,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Reader.Class (MonadReader)
import Lib.App.Env (Env)

-- | @Env@ - Datentyp für die @App@
type AppEnv = Env App

-- | Applikation-Monade
newtype App a = App
  { unApp :: ReaderT AppEnv IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

-- | führt eine @App@ mit der gegebenen @AppEnv@ in IO aus
-- nützlich z.B. um einen Servant-Handler in @App@ zu hoisten
runApp :: AppEnv -> App a -> IO a
runApp env = flip runReaderT env . unApp