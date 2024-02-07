module WorldTest where

import Test.QuickCheck
import World 

-- List of all properties
worldProperties :: [(String, Property)]
worldProperties = [ ("prop_won", prop_won)
             , ("prop_notWon", prop_notWon)
             ]

-- Function to run all properties
runWorldTest :: IO ()
runWorldTest = mapM_ (\(name, prop) -> do
    putStrLn ("Running " ++ name)
    quickCheck prop) worldProperties

-- Define arbitrary GameData
instance Arbitrary GameData where
    arbitrary = do
        location <- arbitrary
        world <- listOf ((,) <$> arbitrary <*> arbitrary)
        items <- listOf arbitrary
        poured <- arbitrary
        caffeinated <- arbitrary
        finished <- arbitrary
        light <- arbitrary
        showered <- arbitrary
        drunk <- arbitrary
        return $ GameData location world items poured caffeinated finished light showered drunk

instance Arbitrary ObjectType where
    arbitrary = elements [Mug, CoffeePot, Laptop]

instance Arbitrary WorldObject where
    arbitrary = do
        objType <- arbitrary
        return $ case objType of
            Mug -> mug
            CoffeePot -> coffeepot
            Laptop -> laptop

instance Arbitrary Direction where
    arbitrary = elements [North, East, South, West, Out, In]

instance Arbitrary RoomID where
    arbitrary = elements [Bedroom, Kitchen, Hall, Street, Lounge]

instance Arbitrary Exit where
    arbitrary = do
        direction <- arbitrary
        description <- arbitrary
        roomId <- arbitrary
        return $ Exit direction description roomId

instance Arbitrary Room where
    arbitrary = do
        roomId <- arbitrary
        description <- arbitrary
        exits <- listOf arbitrary
        objects <- listOf arbitrary
        return $ Room roomId description exits objects

-- Property: If the player is in the street and has a laptop, they should have won
prop_won :: Property
prop_won = forAll arbitrary $ \game_data ->
    (location_id game_data == Street && (laptop `elem` inventory game_data)) === won game_data

-- Property: If the player is not in the Street, they should not have won, regardless of their inventory
prop_notWon :: Property
prop_notWon = forAll arbitrary $ \game_data -> 
  (location_id game_data /= Street) ==> not (won game_data)
