module WorldTest where

import Test.QuickCheck
import World 

-- List of all properties
properties :: [(String, Property)]
properties = [ ("prop_won", prop_won)
             , ("prop_notWon", prop_notWon)
             ]

-- Function to run all properties
runWorldTest :: IO ()
runWorldTest = mapM_ (\(name, prop) -> do
    putStrLn ("Running " ++ name)
    quickCheck prop) properties

-- TODO: These tests will have to be refactored for implementing State

-- Define specific GameData instances for each test
gameDataWon :: GameData
gameDataWon = GameData { location_id = Street, inventory = [laptop]}

gameDataNotWon :: GameData
gameDataNotWon = GameData { location_id = Bedroom, inventory = []}

-- Property: If the player is in the street and has a laptop, they should have won
prop_won :: Property
prop_won = property $ won gameDataWon

-- Property: If the player is not in the Street, they should not have won, regardless of their inventory
prop_notWon :: Property
prop_notWon = property $ not (won gameDataNotWon)
