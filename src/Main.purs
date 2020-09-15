module Main where

import Prelude

import Data.Foldable (traverse_)
import Data.List (List)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
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

interpret :: Command -> Store -> Effect Store
interpret command store =
    case command of
        Put key value ->
           pure (M.insert key value store)
        Get key       -> do
           log (findKey key store)
           pure store
        Delete key    ->
           pure (M.delete key store)
        Dump          -> do
           traverse_ log (getData store)
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

main :: Effect Unit
main = do
    interface <- createConsoleInterface noCompletion
    let store = M.empty
    launchAff_ do
       loop interface store
       liftEffect $ exit 0
  where
    loop :: Interface -> Store -> Aff Unit
    loop interface store = do
       response <- question "λ. " interface
       let mCommand = parse response
       case mCommand of
            Nothing -> do
               log "Could not parse command."
               loop interface store
            Just command -> do
               store' <- liftEffect (interpret command store)
               loop interface store'
