module ActionsTest where

import Control.Monad.State
import Test.QuickCheck
import Data.Maybe (isJust, isNothing)
import Data.List

import Actions
import World

import WorldTest

knownActions :: [String]
knownActions = ["go", "get", "put", "examine", "drink"]

knownCommands :: [String]
knownCommands = ["press", "pour", "open", "quit", "inventory"]

knownArguments :: [String]
knownArguments = ["north", "east", "south", "west", "in", "out", "mug", "coffeepot", "laptop"]

-- List of all properties
actionProperties :: [(String, Property)]
actionProperties = [ ("prop_actions", prop_actions)
             , ("prop_actionsUnknown", prop_actionsUnknown)
             , ("prop_commands", prop_commands)
             , ("prop_commandsUnknown", prop_commandsUnknown)
             , ("prop_arguments", prop_arguments)
             , ("prop_argumentsUnknown", prop_argumentsUnknown)
             -- , ("prop_moveValid", prop_moveValid)
             , ("prop_moveInvalid", prop_moveInvalid)
             , ("prop_moveInvalid", prop_moveInvalid)
             -- , ("prop_updateLocation", prop_updateLocation)
             , ("prop_objectHere", prop_objectHere)
             , ("prop_objectHere", prop_objectHere)
             , ("prop_objectNotHere", prop_objectNotHere)
             , ("prop_addObject", prop_addObject)
             , ("prop_removeObject", prop_removeObject)
             ]

-- Function to run all properties
runActionTests :: IO ()
runActionTests = mapM_ (\(name, prop) -> do
    putStrLn ("Running " ++ name)
    quickCheck prop) actionProperties

-- Test that 'actions' returns a Just value for known inputs and Nothing for unknown inputs
prop_actions :: Property
prop_actions = forAll (elements knownActions) $ \input -> 
    isJust (actions input)

prop_actionsUnknown :: Property
prop_actionsUnknown = forAll (suchThat arbitrary (\input -> not (input `elem` knownActions))) $ \input ->
    isNothing (actions input)

-- Test that 'commands' returns a Just value for known inputs and Nothing for unknown inputs
prop_commands :: Property
prop_commands = forAll (elements knownCommands) $ \input ->
    isJust (commands input)

prop_commandsUnknown :: Property
prop_commandsUnknown = forAll (suchThat arbitrary (\input -> not (input `elem` knownCommands))) $ \input ->
    isNothing (commands input)

-- Test that 'arguments' returns a Just value for known inputs and Nothing for unknown inputs
prop_arguments :: Property
prop_arguments = forAll (elements knownArguments) $ \input -> 
    isJust (arguments input)

prop_argumentsUnknown :: Property
prop_argumentsUnknown = forAll (suchThat arbitrary(\input -> not (input `elem` knownArguments))) $ \input ->
    isNothing (arguments input)

-- prop_moveValid :: Property
-- prop_moveValid = forAll arbitrary $ \room -> forAll (elements $ exits room) $ \exit ->
--   let dir = exit_dir exit in
--   isJust (move dir room)

prop_moveInvalid :: Property
prop_moveInvalid = forAll arbitrary $ \room ->
  let validDirections = map exit_dir $ exits room
      invalidDirections = [North, East, South, West, Out, In] \\ validDirections in
  if null invalidDirections
    then discard -- If there are no invalid directions, discard this test case.
    else forAll (elements invalidDirections) $ \dir ->
      isNothing (move dir room)

prop_objectHere :: Property
prop_objectHere = forAll arbitrary $ \(obj, room) ->
  let roomWithObject = room { objects = obj : objects room }
  in objectHere obj roomWithObject

prop_objectNotHere :: Property
prop_objectNotHere = forAll arbitrary $ \(obj, room) ->
  let roomWithoutObject = room { objects = filter (/= obj) (objects room) }
  in not (objectHere obj roomWithoutObject)

prop_addObject :: Property
prop_addObject = forAll arbitrary $ \(obj, room) ->
  let updatedRoom = addObject obj room
  in obj `elem` objects updatedRoom

prop_removeObject :: Property
prop_removeObject = forAll arbitrary $ \(obj, room) ->
  let updatedRoom = removeObject obj room
  in obj `notElem` objects updatedRoom

-- Property: Moving to a room updates the location_id correctly
-- prop_updateLocation :: Property
-- prop_updateLocation = forAll arbitrary $ \game_data -> forAll arbitrary $ \dir ->
--   case find (\exit -> exit_dir exit == dir) . exits . getRoomData $ game_data of
--     Just exit -> location_id (execState (move dir) game_data) == room exit
--     Nothing -> discard

