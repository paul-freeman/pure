module Test.Main where

import Prelude

import Client (postTo, getFrom)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (delay, runAff_, Milliseconds(..))
--import Test.Assert

main :: Effect Unit
main = do
  let a = {host: "localhost", port: 9140, user: "a", auth: ""}
      b = {host: "localhost", port: 9140, user: "b", auth: ""}
      a_postTo_b = postTo a "b"
      b_postTo_a = postTo b "a"
      a_getFrom_b = getFrom a "b"
      b_getFrom_a = getFrom b "a"
      wait = delay $ Milliseconds (toNumber 3000)
      run = (flip runAff_) wait
  
  a_postTo_b "hello2"
  run (\_ -> do
    b_getFrom_a
    run (\_ -> do
      b_postTo_a "hi! how are you?"
      run (\_ -> do
        a_getFrom_b
        run (\_ -> do
          a_postTo_b "great!"
          run (\_ -> 
            b_getFrom_a)))))
  
  
  
  