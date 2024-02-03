module World where


{-- GameData --}
data GameData = GameData { location_id :: RoomID,      -- where player is
                           world :: [(RoomID, Room)],  -- all possible locations
                           inventory :: [Object],      -- objects player has
                           poured :: Bool,             -- coffee is poured
                           caffeinated :: Bool,        -- coffee is drunk
                           finished :: Bool             -- set to True at the end
                         }




{-- Object --}
data Object = Object { obj_name :: ObjectType,  -- The short name of the object (also its type)
                    obj_longname :: String,  -- The long name of the object
                    obj_desc :: String }     -- A description of the object
    deriving (Eq)

data ObjectType = Mug | CoffeePot | Laptop
    deriving (Eq)


instance Show Object where
    show obj = obj_longname obj

-- Potential addition once fixed
-- instance Eq Object where
--     (==) a b | obj_name a == obj_name b = True
--              | otherwise                = False
--     (==) _ _ = False


mug, fullmug, coffeepot, laptop :: Object
mug       = Object Mug       "a coffee mug"      "A coffee mug"
fullmug   = Object Mug       "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Object CoffeePot "a pot of coffee"   "A pot containing freshly brewed coffee"
laptop    = Object Laptop    "a laptop"          "A laptop for studying"

-- objects = [
--     ()
-- ]





{-- Room --}
data Room = Room {  room_name :: RoomID, -- 
                    room_desc :: String,   -- The name of the room / description of its purpose
                    exits :: [Exit],       -- The exit routes from the current room to another, if applicable
                    objects :: [Object] }  -- The objects contained within the current room
    deriving (Eq)

data RoomID = Bedroom | Kitchen | Hall | Street | Lounge
    deriving (Eq, Show)

instance Show Room where
    show (Room _ desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs

{-- Room Constructors --}
bedroom, kitchen, lounge, hall, street :: Room

bedroom = Room Bedroom                                               -- RoomID
                "You are in your bedroom."                           -- Room description
               [Exit North "To the north is a kitchen. " Kitchen]  -- [Exit]
               [mug]

kitchen = Room Kitchen
                "You are in the kitchen."
               [Exit South "To the south is your bedroom. " Bedroom,
                Exit West "To the west is a hallway. " Hall]
               [coffeepot]

lounge = Room Lounge
              "You are in the lounge."
              []
              [laptop]

hall = Room Hall
            "You are in the hallway. The front door is closed. "
            [Exit East "To the east is a kitchen. " Kitchen]
            []

{-- New data about the hall for when we open the door --}
openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit East "To the east is a kitchen. " Kitchen,
               Exit Out "You can go outside. " Street]

street = Room Street
              "You have made it out of the house."
              [Exit In "You can go back inside if you like. " Hall]
              []

{-- A list of all possible environments that the player could find themselves in --}
gameworld = [(Bedroom, bedroom),
             (Kitchen, kitchen),
             (Hall,    hall),
             (Street,  street)  ]
   



{-- Exit --}
data Exit = Exit { exit_dir :: Direction,   -- The direction of the exit relative to the player's position in the room
                   exit_desc :: String,  -- A description of the exit route
                   room :: RoomID }      -- The name of the room to which the exit leads
    deriving (Eq)





instance Show GameData where
    show game_data = show (getRoomData game_data)

{-- Check if the player has won (i.e. if their current location is the "street") --}
won :: GameData -> Bool
won game_data = location_id game_data == Street

{-- Sets the initial values for the game's state and returns a GameData object representation --}
initState :: GameData
initState = GameData Bedroom gameworld [] False False False

{- Return the room the player is currently in -}
getRoomData :: GameData -> Room
getRoomData game_data = maybe undefined id (lookup (location_id game_data) (world game_data))




{-- An alias for the message returned by actions, indicating success or failure. --}
type ReturnValue = String

{-- Things which do something to an object and update the game state. --}
type Action = Argument -> GameData -> (GameData, ReturnValue)

{-- A required parameter for an Action. --}
data Argument = Obj (Object) | Dir (Direction)

{-- Things which just update game state. --}
type Command = GameData -> (GameData, ReturnValue)

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
    deriving (Eq, Show, Read)
