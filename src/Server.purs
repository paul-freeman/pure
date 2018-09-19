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
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Node.HTTP (Request, Response, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream, requestAsStream, setStatusCode)
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

parseRoute :: Maybe String -> Maybe Route
parseRoute Nothing = Nothing
parseRoute (Just queryString) =
  case concatMap (split (Pattern "=")) (split (Pattern "&") $ drop 1 queryString) of
    ["from", user1, "to", user2] -> Just { from: user1, to: user2 }
    ["to", user2, "from", user1] -> Just { from: user1, to: user2 }
    _ -> Nothing

getMessage :: Request -> Response -> Ref.Ref Messages -> Effect Unit
getMessage req res ref = do
  let query = toMaybe $ _.query $ parse (requestURL req)
  case parseRoute query of
    Just route ->
      respondQuery req res ref route
    Nothing ->
      respondBadQuery req res query

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

respondBadQuery :: Request -> Response -> Maybe String -> Effect Unit
respondBadQuery req res query = do
  let outputStream = responseAsStream res
  setHeader res "Content-Type" "text/plain"
  setStatusCode res 200
  _ <- writeString outputStream UTF8 ("Received bad GET request\n") (pure unit)
  _ <- writeString outputStream UTF8 ("   " <> show query <> "\n") (pure unit)
  end outputStream (pure unit)

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
  log "Hello!!!"
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