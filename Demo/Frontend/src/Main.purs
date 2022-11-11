module Main where

import Prelude

import Api.Echo (echo)
import Components.Main as MainPage
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (error, log)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  -- teste Backend
  res <- echo "Hallo"
  case res of
    Left err -> error $ "Fehler bei Echo: " <> err
    Right echo -> log $ "Echo-Result: " <> echo
  body <- HA.awaitBody
  runUI MainPage.component unit body