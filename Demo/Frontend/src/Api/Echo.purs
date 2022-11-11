module Api.Echo where

import Prelude

import Affjax.ResponseFormat as ResponseFormat
import Affjax.Web (printError)
import Affjax.Web as AJ
import Data.Argonaut (decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Aff (Aff)

echo :: String -> Aff (Either String String)
echo txt = do
  response <- AJ.get (ResponseFormat.json) $ "http://localhost:8081/echo?text=" <> txt
  pure do
    { body: json } <- lmap printError response
    lmap printJsonDecodeError $ decodeJson json