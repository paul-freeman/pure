module Server (runServer, loadMessagesData) where

import Prelude

import Data.Array (concatMap, findIndex, (!!), updateAt, modifyAt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Nullable (toMaybe)
import Data.String (drop, split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Ref (Ref)
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
import Web.Socket.Event.MessageEvent (MessageEvent)

type State =
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
getMessage :: Request -> Response -> Ref State -> Effect Unit
getMessage req res ref =
  case lookup "user" (requestHeaders req) of
    Just userId ->
      case toMaybe $ _.query $ parse (requestURL req) of
        Just query ->
          case split (Pattern "=") (drop 1 query) of
            ["from", user] ->
              respondQuery res ref { from: user, to: userId }

            _ ->
              badRequest "missing 'from' key in query string" res
        Nothing ->
          badRequest "missing '?from=<user-id>' in query" res
    Nothing ->
      badRequest "missing 'user' in header data" res


respondQuery :: Response -> Ref State -> Route -> Effect Unit
respondQuery res ref route = do
  let body = responseAsStream res
  eitherErrStrIndex <- lookupUnreadIndex route ref
  case eitherErrStrIndex of
    Right index -> do
      setHeader res "Content-Type" "text/plain"
      setStatusCode res 200
      maybeMsg <- markDelivered index ref
      case maybeMsg of
        Just msg -> do
          _ <- writeString body UTF8 msg.message (pure unit)
          end body (pure unit)
        Nothing ->
          badRequest "could not retrieve message correctly" res
    Left errStr -> do
      setHeader res "Content-Type" "text/plain"
      setStatusCode res 200
      _ <- writeString body UTF8 "(no new messages)" (pure unit)
      end body (pure unit)


-- | Find an unread message for the given route
lookupUnreadIndex :: Route -> Ref State -> Effect (Either String Int)
lookupUnreadIndex route stateRef = do
  state <- Ref.read stateRef
  case findIndex (isUnread route) state.messages of
    Nothing -> pure $ Left ("No unread messages from " <> route.from <> " to " <> route.to)
    Just index -> pure $ Right index


-- | Marks the indexed message as read and returns it (ignored if index out-of-bounds)
markDelivered :: Int -> Ref State -> Effect (Maybe Message)
markDelivered index stateRef =
  Ref.modify' (markDelivered' index) stateRef

markDelivered' :: Int -> State -> { state :: State, value :: Maybe Message }
markDelivered' index state =
  case modifyAt index (\msg -> msg { delivered = true }) state.messages of
    Just newMessages ->
      { state: state { messages = newMessages }, value: newMessages!!index }
    Nothing ->
      { state: state, value: Nothing }
  

-- | Error 400 Bad Request
badRequest :: String -> Response -> Effect Unit
badRequest = respondWithError 400

-- | Error 405 Method Not Available
methodNotAvailable :: String -> Response -> Effect Unit
methodNotAvailable = respondWithError 405

-- | Send an error response with a specific code
respondWithError :: Int -> String -> Response -> Effect Unit
respondWithError status error res = do
  let out = responseAsStream res
  setHeader res "Content-Type" "text/plain"
  setStatusCode res status
  setStatusMessage res error
  end out (pure unit)

postMessage :: Request -> Response -> Ref.Ref State -> Effect Unit
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

respondToMessage :: Request -> Response -> Ref.Ref State -> Effect Unit
respondToMessage req res ref = do
  case requestMethod req of
    "GET" -> do
      getMessage req res ref
    "POST" -> do
      postMessage req res ref
    _ ->
      methodNotAvailable "Must be GET or POST method" res

respondToNonMessage :: Request -> Response -> Effect Unit
respondToNonMessage req res = do
  let path = _.pathname $ parse $ requestURL req
      outputStream = responseAsStream res
  setHeader res "Content-Type" "text/plain"
  _ <- writeString outputStream UTF8 ("bad path: " <> show path <> "\n") (pure unit)
  end outputStream (pure unit)

respond :: Ref.Ref State -> Request -> Response -> Effect Unit
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

loadMessagesData :: Effect State
loadMessagesData = do
  let filepath = "./state.json"
  dataExists <- exists filepath
  if dataExists
    then do
      dat <- readTextFile UTF8 filepath
      case readJSON dat of
        Left errors -> do
          log "malformed JSON file - starting new state file"
          pure defaultState
        Right (json :: State) ->
          pure json
    else
      pure defaultState

defaultState :: State
defaultState =
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