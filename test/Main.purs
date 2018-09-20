module Test.Main where

import Prelude

import Client (Client, postTo, getFrom)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (delay, runAff_)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Exception (Error)

main :: Effect Unit
main = do
  let paul = {host: "localhost", port: 9140, user: "paul", auth: ""}
      laura = {host: "localhost", port: 9140, user: "laura", auth: ""}
  msg1 paul laura

msg1 :: Client -> Client -> Effect Unit
msg1 paul laura = do
  postTo paul "laura" "hello"
  runAff_ (msg2 paul laura) (delay $ Milliseconds (toNumber 3000))

msg2 :: Client -> Client -> Either Error Unit -> Effect Unit
msg2 paul laura (Left error) =
  logShow error
msg2 paul laura (Right _) = do
  getFrom laura "paul"
  runAff_ (msg3 paul laura) (delay $ Milliseconds (toNumber 3000))

msg3 :: Client -> Client -> Either Error Unit -> Effect Unit
msg3 paul laura (Left error) =
  logShow error
msg3 paul laura (Right _) = do
  getFrom paul "laura"
  --runAff_ msg4 (delay $ Milliseconds (toNumber 3000))
