module Test.Main where

import Prelude

import Data.Either (either)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Main (parseDice, roll)

main :: Effect Unit
main = do
  let y = parseDice "3d10 + 3"
  log $ show y
  x <- (either (\_ -> throw "Parse error") pure y) >>= roll
  log $ show x

