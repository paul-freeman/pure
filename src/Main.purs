module Main (main) where

import Prelude

import Effect (Effect)
import Server (runServer)

main :: Effect Unit
main = runServer "localhost" 9140