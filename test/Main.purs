module Test.Main where

import Prelude

import Client (postTo, getFrom)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (delay, runAff_, Milliseconds(..))
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Test.Assert (assertEqual)


-- | Test creates 2 clients, a and b.
-- | Clients send 3 messages back and forth
main :: Effect Unit
main = do
  let a = {host: "localhost", port: 9140, user: "a", auth: ""}
      b = {host: "localhost", port: 9140, user: "b", auth: ""}
      m1 = "hello"
      m2 = "hi! how are you?"
      m3 = "great!"
      a_postTo_b = postTo a "b"
      b_postTo_a = postTo b "a"
      a_getFrom_b = getFrom a "b"
      b_getFrom_a = getFrom b "a"
      wait = delay $ Milliseconds (toNumber 500)
      run = (flip runAff_) wait
  -- test execution happens here
  a_postTo_b m1
  run (\_ -> do
    msgRef1 <- b_getFrom_a
    run (\_ -> do
      msg1 <- Ref.read msgRef1
      assertEqual { actual: msg1, expected: m1 }
      log $ "a sends to b: " <> msg1
      b_postTo_a m2
      run (\_ -> do
        msgRef2 <- a_getFrom_b
        run (\_ -> do
          msg2 <- Ref.read msgRef2
          assertEqual { actual: msg2, expected: m2 }
          log $ "b sends to a: " <> msg2
          a_postTo_b m3
          run (\_ -> do
            msgRef3 <- b_getFrom_a
            run (\_ -> do
              msg3 <- Ref.read msgRef3
              assertEqual { actual: msg3, expected: m3 }
              log $ "a sends to b: " <> msg3
            ))))))
  
  
  
  