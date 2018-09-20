module Client (Client, postTo, getFrom) where
  
import Prelude

import Data.Options (Options, (:=))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Foreign.Object (singleton)
import Node.Encoding (Encoding(..))
import Node.HTTP.Client (RequestHeaders(..), RequestOptions, Response, auth, headers, hostname, method, path, port, protocol, statusCode, rejectUnauthorized, request, requestAsStream, responseAsStream, statusMessage)
import Node.Stream (end, onError, onDataString, onEnd, writeString)

type Client =
  { host :: String
  , port :: Int
  , user :: String
  , auth :: String
  }


-- | client posts message to user
postTo :: Client -> String -> String -> Effect Unit
postTo client toUser message = do
  let options =
        hostname := client.host <> port := client.port <> auth := client.auth
      config =
        makeConfig options client.user "POST" toUser
  postMessage config message

-- | client gets message from user
getFrom :: Client -> String -> Effect Unit
getFrom client fromUser = do
  let options =
        hostname := client.host <> port := client.port <> auth := client.auth
      config =
        makeConfig options client.user "GET" fromUser
  getMessage config


-- | add standard options to request config
makeConfig :: Options RequestOptions -> String -> String -> String -> Options RequestOptions
makeConfig options client method_ user =
  let query = (if method_ == "GET" then "from=" else "to=") <> user in
  options <>
  protocol := "http:" <>
  method := method_ <>
  path := ("/message?" <> query) <>
  rejectUnauthorized := false <>
  headers := (RequestHeaders (singleton "user" client))


-- | make the POST request
postMessage :: Options RequestOptions -> String -> Effect Unit
postMessage config msg = do
  req <- request config showRes
  let body = requestAsStream req
  onError body $ showErr "error sending POST request to server"
  _ <- writeString body UTF8 msg (pure unit)
  end body (pure unit)

-- | make the GET request
getMessage :: Options RequestOptions -> Effect Unit
getMessage config = do
  req <- request config showRes
  let body = requestAsStream req
  onError body $ showErr "error sending GET request to server"
  end body (pure unit)


-- | Show the server response
showRes :: Response -> Effect Unit
showRes res = do
  case statusCode res of
    200 -> do
      let message = responseAsStream res
      onDataString message UTF8 log
      onEnd message (pure unit)
    400 ->
      log $ "400 Bad Request\n" <> (statusMessage res)
    405 ->
      log $ "405 Method Not Allowed\n" <> (statusMessage res)
    code ->
      log $ (show code) <> " Unhandled Error\n" <> (statusMessage res)

showErr :: String -> Error -> Effect Unit
showErr errStr error = log $ (show error) <> "\n" <> errStr