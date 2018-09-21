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


-- | Type to hold the client connection data
type Client =
  { host :: String
  , port :: Int
  , user :: String
  , auth :: String
  }


-- | Post a message to another user
postTo :: Client -> String -> String -> Effect Unit
postTo client toUser message = postMessage (makeConfig client "POST" toUser) message
  where
    postMessage config msg = do
      req <- request config showRes
      let body = requestAsStream req
      onError body $ showErr "error sending POST request to server"
      _ <- writeString body UTF8 msg (pure unit)
      end body (pure unit) 

-- | Get a message from another user
getFrom :: Client -> String -> Effect Unit
getFrom client fromUser = getMessage (makeConfig client "GET" fromUser)
  where
    getMessage config = do
      req <- request config showRes
      let body = requestAsStream req
      onError body $ showErr "error sending GET request to server"
      end body (pure unit)
    

-- | make RequestOptions for HTTP request
makeConfig :: Client -> String -> String -> Options RequestOptions
makeConfig client method_ user =
  hostname := client.host <>
  port := client.port <>
  auth := client.auth <>
  protocol := "http:" <>
  method := method_ <>
  path := ("/message?" <> query) <>
  rejectUnauthorized := false <>
  headers := (RequestHeaders (singleton "user" client.user))
    where
      query = (if method_ == "GET" then "from=" else "to=") <> user


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


-- | Show errors
showErr :: String -> Error -> Effect Unit
showErr errStr error = log $ (show error) <> "\n" <> errStr