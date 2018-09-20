module Server (runServer, loadMessagesData) where

import Prelude

import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef
import Data.Array (concatMap, findIndex, (!!), updateAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.String (drop, split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Ref as Ref
import Foreign.Object (lookup)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Node.HTTP (Request, Response, listen, createServer, setHeader, setStatusMessage, requestMethod, requestHeaders, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (end, onDataString, onEnd, onFinish, writeString)
import Node.URL (Query, parse)
import Partial.Unsafe (unsafeCrashWith)
import Simple.JSON (readJSON)
import Type.Data.Boolean (kind Boolean)

type Messages =
  { messages :: Array Message}
  
type Message =
  { from :: String
  , to :: String
  , message :: String
  , delivered :: Boolean
  }

type Route =
  { from :: String
  , to :: String
  }

-- | Respond to a GET message request
getMessage :: Request -> Response -> Ref.Ref Messages -> Effect Unit
getMessage req res ref =
  case lookup "user" (requestHeaders req) of
    Just userId ->
      case toMaybe $ _.query $ parse (requestURL req) of
        Just query ->
          case split (Pattern "=") (drop 1 query) of
            ["from", user] ->
              respondQuery req res ref { from: user, to: userId }

            _ ->
              error400 "missing 'from' key in query string" res
        Nothing ->
          error400 "missing '?from=<user-id>' in query" res
    Nothing ->
      error400 "missing 'user' in header data" res


respondQuery :: Request -> Response -> Ref.Ref Messages -> Route -> Effect Unit
respondQuery req res ref route = do
  let outputStream = responseAsStream res
  setHeader res "Content-Type" "text/plain"
  setStatusCode res 200
  dat <- Ref.read ref
  case findIndex (isUnread route) dat.messages of
    Nothing -> do
      _ <- writeString outputStream UTF8 "   (none)\n" (pure unit)
      end outputStream (pure unit)
    Just idx ->
      case dat.messages!!idx of
        Nothing -> do
          _ <- writeString outputStream UTF8 "   (none)\n" (pure unit)
          end outputStream (pure unit)
        Just msg -> do
          _ <- writeString outputStream UTF8 (route.from <> ":\n") (pure unit)
          _ <- writeString outputStream UTF8 ("   " <> msg.message <> "\n") (pure unit)
          let newMessages = case updateAt idx (msg { delivered = true }) dat.messages of
                              Nothing -> dat.messages
                              Just m -> m
          _ <- Ref.write (dat { messages = newMessages }) ref
          end outputStream (pure unit)

-- | 400 Bad Request
error400 :: String -> Response -> Effect Unit
error400 = respondWithError 400

-- | Send an error response with a specific code
respondWithError :: Int -> String -> Response -> Effect Unit
respondWithError status error res = do
  let out = responseAsStream res
  setStatusCode res status
  setStatusMessage res error
  end out (pure unit)

postMessage :: Request -> Response -> Ref.Ref Messages -> Effect Unit
postMessage req res ref = do
  msg_buf <- Ref.new ""
  let inputMessage = requestAsStream req
      outputStream = responseAsStream res
      query = toMaybe $ _.query $ parse (requestURL req)
      postData = requestAsStream req
      handleData str = Ref.modify_ (_ <> str) msg_buf
      handleEnd = do
        msg <- Ref.read msg_buf
        _ <- writeString outputStream UTF8 ("msg: " <> show msg <> "\n") (pure unit)
        end outputStream (pure unit)
  setHeader res "Content-Type" "text/plain"
  setStatusCode res 200
  _ <- writeString outputStream UTF8 ("Received POST request\n") (pure unit)
  _ <- writeString outputStream UTF8 (show query <> "\n") (pure unit)
  onDataString postData UTF8 handleData
  onEnd postData handleEnd

respondToMessage :: Request -> Response -> Ref.Ref Messages -> Effect Unit
respondToMessage req res ref = do
  case requestMethod req of
    "GET" -> do
      getMessage req res ref
    "POST" -> do
      postMessage req res ref
    _ ->
      unsafeCrashWith "Must be GET or POST method"

respondToNonMessage :: Request -> Response -> Effect Unit
respondToNonMessage req res = do
  let path = _.pathname $ parse $ requestURL req
      outputStream = responseAsStream res
  setHeader res "Content-Type" "text/plain"
  _ <- writeString outputStream UTF8 ("bad path: " <> show path <> "\n") (pure unit)
  end outputStream (pure unit)

respond :: Ref.Ref Messages -> Request -> Response -> Effect Unit
respond ref req res = do
  case toMaybe $ _.pathname $ parse $ requestURL req of
    Just "/message" ->
      respondToMessage req res ref
    _ ->
      respondToNonMessage req res

runServer :: Effect Unit
runServer = do
  let host = "localhost"
      port = 6137
  dat <- loadMessagesData
  state <- Ref.new dat
  server <- createServer $ respond state
  listen server { hostname: host, port: port, backlog: Nothing } (pure unit)

loadMessagesData :: Effect Messages
loadMessagesData = do
  let filepath = "./messages.json"
  dataExists <- exists filepath
  if dataExists
    then do
      dat <- readTextFile UTF8 "./messages.json"
      case readJSON dat of
        Left errors -> do
          log "malformed JSON file - starting new log"
          pure defaultMessages
        Right (json :: Messages) ->
          pure json
    else
      pure defaultMessages

defaultMessages :: Messages
defaultMessages =
  {
      "messages": [
          {
              "from": "system1",
              "to": "system2",
              "message": "test",
              "delivered": false
          }
      ]
  }

isUnread :: Route -> Message -> Boolean
isUnread route message =
  (route.from == message.from) && (route.to == message.to) && (not message.delivered)