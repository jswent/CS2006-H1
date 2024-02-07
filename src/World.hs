{-# LANGUAGE DeriveGeneric #-}

module World where

import GHC.Generics
import Data.Aeson
import Control.Monad.State



{-- Action and Command --}
{-- Actions are things which do something to an object and update the game state. --}
type Action = Argument -> State GameData ReturnValue

{-- A required parameter for an Action. --}
data Argument = ObjArg (WorldObject) 
              | DirArg (Direction)
    deriving (Generic)

{-- An alias for the message returned by actions, indicating success or failure. --}
type ReturnValue = String

{-- Commands are things which just update game state. --}
type Command = State GameData ReturnValue

{-- 
    A type to describe the direction of movement relative to the current position.
    Implements:
        Eq   - Directions must be comparable
        Show - We must be able to print a direction to stdout
        Read - We must be able to read in content from stdin and associate it with the correct type
--}
data Direction = North 
               | East 
               | South 
               | West
               | Out
               | In
    deriving (Eq, Generic, Show, Read)




{-- GameData --}
data GameData = GameData { location_id :: RoomID,      -- Where player is
                           world :: [(RoomID, Room)],  -- All possible locations
                           inventory :: [WorldObject], -- Objects player has
                           poured :: Bool,             -- Coffee is poured
                           caffeinated :: Bool,        -- Coffee is drunk
                           finished :: Bool,            -- Set to True at the end
                           light :: Bool,               -- Light is on
                           showered :: Bool,            -- Player has showered
                           drunk :: Bool               -- Player is drunk
                         }
    deriving (Generic)


instance Show GameData where
    show game_data
      | location_id game_data == Street && drunk game_data = "You are drunk, Drink another coffee to sober up"
      | light game_data = show (getRoomData game_data)
      | otherwise       = "The light is off so you cannot see any exits or objects.\n" ++ room_desc (getRoomData game_data)

instance ToJSON GameData where
    -- No need to provide a toJSON implementation.
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Room where
    -- No need to provide a toJSON implementation.
    toEncoding = genericToEncoding defaultOptions
instance ToJSON WorldObject where
    -- No need to provide a toJSON implementation.
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Exit where
    -- No need to provide a toJSON implementation.
    toEncoding = genericToEncoding defaultOptions
instance ToJSON RoomID where
    -- No need to provide a toJSON implementation.
    toEncoding = genericToEncoding defaultOptions
instance ToJSON ObjectType where
    -- No need to provide a toJSON implementation.
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Direction where
    -- No need to provide a toJSON implementation.
    toEncoding = genericToEncoding defaultOptions
instance FromJSON GameData
    -- No need to provide a parseJSON implementation.
instance FromJSON Room
    -- No need to provide a parseJSON implementation.
instance FromJSON WorldObject
    -- No need to provide a parseJSON implementation.
instance FromJSON Exit
    -- No need to provide a parseJSON implementation.
instance FromJSON RoomID
    -- No need to provide a parseJSON implementation.
instance FromJSON ObjectType
    -- No need to provide a parseJSON implementation.
instance FromJSON Direction
    -- No need to provide a parseJSON implementation.




{-- Object --}
data WorldObject = WorldObject { obj_name :: ObjectType,  -- The short name of the object (also its type)
                                 obj_longname :: String,  -- The long name of the object
                                 obj_desc :: String }     -- A description of the object
    deriving (Eq, Generic)


data ObjectType = Mug | CoffeePot | Laptop | Beer
    deriving (Eq, Generic)


instance Show WorldObject where
    show obj = obj_longname obj


mug, fullmug, coffeepot, laptop :: WorldObject
mug       = WorldObject Mug       "a coffee mug (\"mug\")"          "A coffee mug (\"mug\")"
fullmug   = WorldObject Mug       "a full coffee mug (\"mug\")"     "A coffee mug containing freshly brewed coffee (\"mug\")"
coffeepot = WorldObject CoffeePot "a pot of coffee (\"coffeepot\")" "A pot containing freshly brewed coffee (\"coffeepot\")"
beer      = WorldObject Beer      "a bottle of beer"  "A bottle of ice cold beer (\"beer\")"
laptop    = WorldObject Laptop    "a laptop (\"laptop\")"           "A laptop used for studying (\"laptop\")"




{-- Room --}
data Room = Room {  room_name :: RoomID,        -- The name of the room
                    room_desc :: String,        -- The description of the room's purpose
                    exits :: [Exit],            -- The exit routes from the current room to another, if applicable
                    objects :: [WorldObject] }  -- The objects contained within the current room
    deriving (Eq, Generic)

data RoomID = Bedroom | Kitchen | Hall | Street | Lounge | Bathroom
    deriving (Eq, Generic, Show, Read)


instance Show Room where
    show (Room _ desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
             
             
           
           
{-- Room Constructors --}
bedroom, kitchen, lounge, hall, street, bathroom :: Room

bedroom = Room Bedroom                                            -- RoomID
                "You are in your bedroom. "                       -- Room description
               [Exit North "To the north is a kitchen. "   Kitchen, -- [Exit]
                Exit West  "To the west is the lounge. "   Lounge,
                Exit East  "To the east is the bathroom. " Bathroom]
               [mug]                                              -- [WorldObject]

kitchen = Room Kitchen
                "You are in the kitchen."
               [Exit South "To the south is your bedroom. " Bedroom,
                Exit West "To the west is a hallway. " Hall]
               [coffeepot, beer]


{-- New data about the lounge for when we turn the light on is below this "lounge" constructor --}
lounge = Room Lounge
              "You are in the lounge. The light switch is off. (Use the \"press\" command to turn on the lights)"
              [Exit East "To the east is a bedroom. " Bedroom]
              [laptop]

bathroom = Room Bathroom
                "You are in the Bathroom. You have not showered today"
                [Exit West "To the west is your bedroom. " Bedroom]
                []

litloungedesc = "You are in the lounge. The light switch is on"
bathroomShoweredDesc = "You are in the bathroom. You have showered. "


{-- New data about the hall for when we open the door is below this "hall" constructor --}
hall = Room Hall
            "You are in the hallway. The front door is closed. "
            [Exit East "To the east is a kitchen. " Kitchen]
            []
            
openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit East "To the east is a kitchen. " Kitchen,
               Exit Out "You can go outside. " Street]


street = Room Street
              "You have made it out of the house. To finish the game you must bring your laptop to the lecture and take a shower."
              [Exit In "You can go back inside if you like. " Hall]
              []


{-- A list of all possible environments that the player could find themselves in --}
gameworld = [(Bedroom,  bedroom),
             (Kitchen,  kitchen),
             (Hall,     hall),
             (Street,   street),
             (Lounge,   lounge),
             (Bathroom, bathroom)]


{-- Exit --}
data Exit = Exit { exit_dir :: Direction,   -- The direction of the exit relative to the player's position in the room
                   exit_desc :: String,  -- A description of the exit route
                   room :: RoomID }      -- The name of the room to which the exit leads
    deriving (Eq, Generic)




{-- Functions inportant for the game logic. --}
{-- Check if the player has won (i.e. if their current location is the "street", and they have their laptop with them) --}
won :: GameData -> Bool
won game_data = location_id game_data == Street && (laptop `elem` inventory game_data) && not (drunk game_data) && showered game_data == True

{-- Sets the initial values for the game's state and returns a GameData object representation --}
initState :: GameData
initState = GameData Bedroom gameworld [] False False False False False False

{- Return the room the player is currently in -}
getRoomData :: GameData -> Room
getRoomData game_data = maybe undefined id (lookup (location_id game_data) (world game_data))
