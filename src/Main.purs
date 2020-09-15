module Main where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List (List)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Options ((:=))
import Data.String (Pattern(..), split, stripSuffix)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.Encoding as Encoding
import Node.Net.Server as Server
import Node.Net.Socket as Net
import Node.Process (exit)
import Node.ReadLine (createConsoleInterface, noCompletion)
import Node.ReadLine.Aff (Interface, question)

data Command
    = Put String String
    | Get String
    | Delete String
    | Dump
    | Exit

type Store = Map String String

interpret :: Command -> Store -> (String -> Effect Unit) -> Effect Store
interpret command store output =
    case command of
        Put key value ->
           pure (M.insert key value store)
        Get key       -> do
           output (findKey key store)
           pure store
        Delete key    ->
           pure (M.delete key store)
        Dump          -> do
           traverse_ output (getData store)
           pure store
        Exit          ->
           exit 0

findKey :: String -> Store -> String
findKey key store =
    fromMaybe "No key found" (M.lookup key store)

getData :: Store -> List String
getData store =
    go <$> M.toUnfoldable store
  where
    go :: Tuple String String -> String
    go (Tuple key value) = key <> " => " <> value

parse :: String -> Maybe Command
parse input
  | input == "exit" = Just Exit
  | input == "dump" = Just Dump
  | otherwise       =
    case split (Pattern " ") input of
         [ "get", key ]        -> Just (Get key)
         [ "delete", key ]     -> Just (Delete key)
         [ "put", key, value ] -> Just (Put key value)
         _ -> Nothing

runServer :: Ref Store -> Effect Unit
runServer ref = do
    server <- Server.createServer mempty connectionHandler
    Server.listen
        server
        ( Server.listenPort := 4000 )
        ( pure unit )
  where
    connectionHandler :: Net.Socket -> Effect Unit
    connectionHandler socket =
        Net.onData socket $ \input -> do
            str <- case input of
                Left buffer -> Buffer.toString Encoding.UTF8 buffer
                Right str   -> pure str
            let mCommand = parse (fromMaybe str $ stripSuffix (Pattern "\n") str)
            case mCommand of
                Nothing -> log "Could not parse command."
                Just command -> do
                   store <- Ref.read ref
                   store' <- interpret
                               command
                               store
                               (\s -> do
                                   _ <- Net.writeString
                                       socket
                                       (s <> "\n")
                                       Encoding.UTF8
                                       mempty
                                   pure unit
                               )
                   Ref.write store' ref
                   pure unit

main :: Effect Unit
main = do
    interface <- createConsoleInterface noCompletion
    store <- Ref.new M.empty
    launchAff_ do
       liftEffect (runServer store)
       loop interface store
       liftEffect $ exit 0
  where
    loop :: Interface -> Ref Store -> Aff Unit
    loop interface ref = do
       response <- question "λ. " interface
       let mCommand = parse response
       case mCommand of
            Nothing -> do
               log "Could not parse command."
               loop interface ref
            Just command -> do
               liftEffect do
                   store <- Ref.read ref
                   store' <- interpret command store log
                   Ref.write store' ref
               loop interface ref
