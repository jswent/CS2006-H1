module ActionsTest where

import Test.QuickCheck
import Data.Maybe (isJust, isNothing)

import Actions

knownActions :: [String]
knownActions = ["go", "get", "put", "examine", "drink"]

knownCommands :: [String]
knownCommands = ["press", "pour", "open", "quit", "inventory"]

knownArguments :: [String]
knownArguments = ["north", "east", "south", "west", "in", "out", "mug", "coffeepot", "laptop"]

-- List of all properties
properties :: [(String, Property)]
properties = [ ("prop_actions", prop_actions)
             , ("prop_actionsUnknown", prop_actionsUnknown)
             , ("prop_commands", prop_commands)
             , ("prop_commandsUnknown", prop_commandsUnknown)
             , ("prop_arguments", prop_arguments)
             , ("prop_argumentsUnknown", prop_argumentsUnknown)
             ]

-- Function to run all properties
runActionTests :: IO ()
runActionTests = mapM_ (\(name, prop) -> do
    putStrLn ("Running " ++ name)
    quickCheck prop) properties

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
