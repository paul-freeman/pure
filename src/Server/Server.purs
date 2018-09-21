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


-- | State is just messages, but left as record for expansion purposes
-- |
-- | State is saved to a JSON file for simplicity
type State =
  { messages :: Array Message}

-- | Message type saves details about the message
-- | and if it has been delivered or not
type Message =
  { from :: String
  , to :: String
  , message :: String
  , delivered :: Boolean
  }

-- | Routes are used to search for messages in the state
type Route =
  { from :: String
  , to :: String
  }


-- | The main server - loads state and listens for requests
runServer :: String -> Int -> Effect Unit
runServer host port = do
  dat <- loadMessagesData
  state <- Ref.new dat
  server <- createServer $ respond state
  listen server { hostname: host, port: port, backlog: Nothing } (pure unit)


-- | Server response function
-- | Verifies the URI of the request
respond :: Ref State -> Request -> Response -> Effect Unit
respond ref req res = do
  case toMaybe $ _.pathname $ parse $ requestURL req of
    Just "/message" -> do
      case requestMethod req of
        "GET" -> do
          getMessage req res ref
        "POST" -> do
          postMessage req res ref
        _ ->
          methodNotAvailable "Must be GET or POST method" res
    _ ->
      badRequest ("bad path: " <> show path) res
        where 
          path = _.pathname $ parse $ requestURL req


-- | Get a message from the server and mark it as delivered
getMessage :: Request -> Response -> Ref.Ref State -> Effect Unit
getMessage req res stateRef = do
  setHeader res "Content-Type" "text/plain"
  setStatusCode res 200
  case parseGet req of
    Left errStr ->
      badRequest errStr res
    Right route -> do
      let body = responseAsStream res
      eitherErrStrIndex <- lookupUnreadIndex route stateRef
      case eitherErrStrIndex of
        Right index -> do
          maybeMsg <- markDelivered index stateRef
          state <- Ref.read stateRef
          _ <- saveMessageData state
          case maybeMsg of
            Just msg -> do
              _ <- writeString body UTF8 msg.message (pure unit)
              end body (pure unit)
            Nothing ->
              badRequest "could not retrieve message correctly" res
        Left errStr -> do
          _ <- writeString body UTF8 "(no new messages)" (pure unit)
          end body (pure unit)

-- | Receive and store a new message at the server
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


-- | Parse a route from a request
parseGet :: Request -> Either String Route
parseGet = parseReq "from"

-- | Parse a POST request
parsePost :: Request -> Either String Route
parsePost = parseReq "to"

-- | Parse the GET request
parseReq :: String -> Request -> Either String Route
parseReq key req  =
  case lookup "user" (requestHeaders req) of
    Just userId ->
      case toMaybe $ _.query $ parse (requestURL req) of
        Just query ->
          case split (Pattern "=") query of
            [key', user] ->
              if key' == "from"
                then Right { from: user, to: userId }
                else
                  if key' == "to"
                    then Right { from: userId, to: user }
                    else Left $ "missing '" <> key <> "' key in query string"
            _ ->
              Left $ "missing '" <> key <> "' key in query string"
        Nothing ->
          Left $ "missing '?" <> key <> "=<user-id>' in query"
    Nothing ->
      Left $ "missing 'user' in header data"


-- HELPER FUNCTIONS

-- | Find an unread message for the given route
lookupUnreadIndex :: Route -> Ref State -> Effect (Either String Int)
lookupUnreadIndex route stateRef = do
  state <- Ref.read stateRef
  case findIndex (isUnread route) state.messages of
    Nothing -> pure $ Left ("No unread messages from " <> route.from <> " to " <> route.to)
    Just index -> pure $ Right index
      where
        isUnread route message =
          (route.from == message.from) && (route.to == message.to) && (not message.delivered)

-- | Marks the indexed message as read and returns it (ignored if index out-of-bounds)
markDelivered :: Int -> Ref State -> Effect (Maybe Message)
markDelivered index stateRef =
  Ref.modify' (markDelivered' index) stateRef
    where
      markDelivered' index' state =
        case modifyAt index' (\msg -> msg { delivered = true }) state.messages of
          Just newMessages ->
            { state: state { messages = newMessages }, value: newMessages!!index' }
          Nothing ->
            { state: state, value: Nothing }

-- | Add new message to the state
addMessage :: Message -> Ref State -> Effect Unit
addMessage msg stateRef =
  Ref.modify_ (addMessage' msg) stateRef
    where  
      addMessage' msg' state =
        case insertAt (length state.messages) msg' state.messages of
          Just newMessages ->
            state { messages = newMessages }
          Nothing ->
            state


-- ERROR RESPONSES

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


-- PERSISTENT STORAGE

filepath :: String
filepath = "./state.json"

defaultState :: State
defaultState = {"messages": []}

-- | Load state data, creating empty state on error
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

-- | Write data to disk
-- | Currently called after each server request
saveMessageData :: State -> Effect Unit
saveMessageData = writeJSON >>> writeTextFile UTF8 filepath
