module Main where

import Prelude

import Data.Foldable (traverse_)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
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

type Store = List (Tuple String String)

interpret :: Command -> Store -> Effect Store
interpret command store =
    case command of
        Put key value -> pure (updateStore key value store)
        Get key       -> do
           log (findKey key store)
           pure store
        Delete key    ->
           pure (deleteKey key store)
        Dump          -> do
           traverse_ log (getData store)
           pure store
        Exit          -> exit 0

updateStore :: String -> String -> Store -> Store
updateStore key value store =
    case store of
        Nil -> Cons (Tuple key value) Nil
        Cons head@(Tuple currentKey _) rest ->
            if currentKey == key
                then Cons (Tuple key value) rest
                else Cons head (updateStore key value rest)

findKey :: String -> Store -> String
findKey key store =
    case store of
         Nil -> "No key found"
         Cons (Tuple currentKey value) rest ->
             if currentKey == key
                 then value
                 else findKey key rest

deleteKey :: String -> Store -> Store
deleteKey key store =
    case store of
         Nil -> Nil
         Cons head@(Tuple currentKey _) rest ->
              if currentKey == key
                  then rest
                  else Cons head (deleteKey key rest)

getData :: Store -> List String
getData store =
    case store of
         Nil -> Nil
         Cons (Tuple key value) rest ->
             Cons (key <> " => " <> value) (getData rest)

parse :: String -> Maybe Command
parse input
  | input == "exit" = Just Exit
  | input == "dump" = Just Dump
  | otherwise       =
    case split (Pattern " ") input of
         [ "get", key ] -> Just (Get key)
         [ "delete", key ] -> Just (Delete key)
         [ "put", key, value ] -> Just (Put key value)
         _ -> Nothing

main :: Effect Unit
main = do
    interface <- createConsoleInterface noCompletion
    let store = Nil
    launchAff_ do
       _ <- loop interface store
       liftEffect $ exit 0
  where
    loop :: Interface -> Store -> Aff Store
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
