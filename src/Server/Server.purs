module Server (runServer) where

import Prelude

import Data.Array (findIndex, (!!), modifyAt, insertAt, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Foreign.Object (lookup)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (exists, readTextFile, writeTextFile)
import Node.HTTP (Request, Response, listen, createServer, setHeader, setStatusMessage, requestMethod, requestHeaders, requestURL, responseAsStream, requestAsStream, setStatusCode)
import Node.Stream (end, onDataString, onEnd, writeString)
import Node.URL (parse)
import Simple.JSON (readJSON, writeJSON)

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

-- | Parse the GET request
parseGet :: Request -> Response -> Ref State -> Effect Unit
parseGet req res ref =
  case lookup "user" (requestHeaders req) of
    Just userId ->
      case toMaybe $ _.query $ parse (requestURL req) of
        Just query ->
          case split (Pattern "=") query of
            ["from", user] ->
              respondQuery res ref { from: user, to: userId }
            _ ->
              badRequest "missing 'from' key in query string" res
        Nothing ->
          badRequest "missing '?from=<user-id>' in query" res
    Nothing ->
      badRequest "missing 'user' in header data" res


-- | Parse the POST request
parsePost :: Request -> Either String Route
parsePost req =
  case lookup "user" (requestHeaders req) of
    Just userId ->
      case toMaybe $ _.query $ parse (requestURL req) of
        Just query ->
          case split (Pattern "=") query of
            ["to", user] ->
              Right { from: userId, to: user }
            _ ->
              Left "missing 'to' key in query string"
        Nothing ->
          Left "missing '?to=<user-id>' in query"
    Nothing ->
      Left "missing 'user' in header data"


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


-- | Add new message to the state
addMessage :: Message -> Ref State -> Effect Unit
addMessage msg stateRef =
  Ref.modify_ (addMessage' msg) stateRef
  
addMessage' :: Message -> State -> State
addMessage' msg state =
  case insertAt (length state.messages) msg state.messages of
    Just newMessages ->
      state { messages = newMessages }
    Nothing ->
      state


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


-- | Receive a new message at the server
postMessage :: Request -> Response -> Ref.Ref State -> Effect Unit
postMessage req res stateRef = do
  case parsePost req of
    Left errStr ->
      badRequest errStr res
    Right route -> do
      msgRef <- Ref.new ""
      let in_ = requestAsStream req
          out = responseAsStream res
          handleData str =
            Ref.modify_ (_ <> str) msgRef
          handleEnd = do
            msg <- Ref.read msgRef
            let message = { from: route.from, to: route.to, message: msg, delivered: false}
            _ <- addMessage message stateRef
            state <- Ref.read stateRef
            _ <- saveMessageData state
            end out (pure unit)
      setHeader res "Content-Type" "text/plain"
      setStatusCode res 200
      onDataString in_ UTF8 handleData
      onEnd in_ handleEnd

respondToMessage :: Request -> Response -> Ref.Ref State -> Effect Unit
respondToMessage req res ref = do
  case requestMethod req of
    "GET" -> do
      parseGet req res ref
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

respond :: Ref State -> Request -> Response -> Effect Unit
respond ref req res = do
  case toMaybe $ _.pathname $ parse $ requestURL req of
    Just "/message" ->
      respondToMessage req res ref
    _ ->
      respondToNonMessage req res

runServer :: String -> Int -> Effect Unit
runServer host port = do
  dat <- loadMessagesData
  state <- Ref.new dat
  server <- createServer $ respond state
  listen server { hostname: host, port: port, backlog: Nothing } (pure unit)

filepath :: String
filepath = "./state.json"

loadMessagesData :: Effect State
loadMessagesData = do
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

saveMessageData :: State -> Effect Unit
saveMessageData = writeJSON >>> writeTextFile UTF8 filepath

defaultState :: State
defaultState = {"messages": []}

isUnread :: Route -> Message -> Boolean
isUnread route message =
  (route.from == message.from) && (route.to == message.to) && (not message.delivered)