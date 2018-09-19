module Client (sendMessage, getMessages) where
  
import Prelude

import Data.Options (Options, (:=))
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (RequestOptions, Response, hostname, method, path, port, protocol, rejectUnauthorized, request, requestAsStream, responseAsStream)
import Node.Stream (Writable, end, pipe, writeString)

foreign import stdout :: forall r. Writable r

config :: String -> String -> Options RequestOptions
config method' query =
  protocol := "http:" <>
  method := method' <>
  hostname := "localhost" <>
  port := 6137 <>
  path := ("/message?" <> query) <>
  rejectUnauthorized := false

sendMessage :: String -> String -> String -> Effect Unit
sendMessage fromUser toUser msg = do
  req <- request (config "POST" $ "?from=" <> fromUser <> "&to=" <> toUser) logResponse
  let body = requestAsStream req
  _ <- writeString body UTF8 msg (pure unit)
  end body (pure unit)

getMessages :: String -> String -> Effect Unit
getMessages toUser fromUser = do
  req <- request (config "GET" $ "?to=" <> toUser <> "&from=" <> fromUser) logResponse
  let body = requestAsStream req
  end body (pure unit)

logResponse :: Response -> Effect Unit
logResponse response = void do
  log "Response:"
  let responseStream = responseAsStream response
  pipe responseStream stdout