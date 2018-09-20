module Client (sendMessage, getMessage) where
  
import Prelude

import Control.Monad.Gen (resize)
import Data.Options (Options, (:=), opt)
import Effect (Effect)
import Effect.Console (log)
import Foreign.Object (singleton)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (RequestHeaders(..), RequestOptions, Response, auth, headers, hostname, method, path, port, protocol, statusCode, rejectUnauthorized, request, requestAsStream, responseAsStream, statusMessage)
import Node.Stream (Writable, end, onFinish, onDataString, onEnd, pipe, writeString)

config :: String -> String -> String -> String -> Options RequestOptions
config method' user auth' query =
  protocol := "http:" <>
  method := method' <>
  hostname := "localhost" <>
  port := 6137 <>
  auth := auth' <>
  path := ("/message?" <> query) <>
  rejectUnauthorized := false <>
  headers := (RequestHeaders (singleton "user" user))

sendMessage :: String -> String -> String -> Effect Unit
sendMessage fromUser toUser msg = do
  req <- request (config "POST" fromUser "" $ "?to=" <> toUser) showRes
  let body = requestAsStream req
  _ <- writeString body UTF8 msg (pure unit)
  end body (pure unit)

-- | GET next unread message from a specific user
getMessage :: String -> String -> Effect Unit
getMessage toUser fromUser = do
  req <- request (config "GET" toUser "" $ "?from=" <> fromUser) showRes
  let body = requestAsStream req
  end body (pure unit)


-- | Show the server response
showRes :: Response -> Effect Unit
showRes res = do
  case statusCode res of
    400 ->
      log $ statusMessage res
    _ -> do
      let message = responseAsStream res
      onDataString message UTF8 log
      onEnd message (pure unit)