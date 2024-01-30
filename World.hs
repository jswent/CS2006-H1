module World where


data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving Eq

{-data Direction = North | South | East | West
   deriving (Eq, Show)

data Command = 
   Move Direction
   | Get Object
   | Put Object
   | Pour Object
   | Examine Object
   | Drink Object
   | Open Object
   | Quit
   | Inv
   deriving (Eq, Show)-}


instance Show Object where
   show obj = obj_longname obj

data Exit = Exit { exit_dir :: String,
                   exit_desc :: String,
                   room :: String }
   deriving Eq

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving Eq

data GameData = GameData { location_id :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           caffeinated :: Bool, -- coffee is drunk
                           finished :: Bool, -- set to True at the end
                           light :: Bool -- Light is on
                         }

won :: GameData -> Bool
won gd = location_id gd == "street" && (laptop `elem` inventory gd)

instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
                                  

instance Show GameData where
    show gd
      | light gd = show (getRoomData gd)
      | otherwise = "The light is off so you cannot see any exits or objects.\n" ++ room_desc (getRoomData gd)

-- Things which do something to an object and update the game state
type Action  = String -> GameData -> (GameData, String)

-- Things which just update the game state
type Command = GameData -> (GameData, String)

mug, fullmug, coffeepot, laptop :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"
laptop    = Obj "laptop" "a laptop" "A laptop used for studying"

bedroom, kitchen, hall, street, lounge :: Room

bedroom = Room "You are in your bedroom."
               [Exit "north" "To the north is a kitchen. " "kitchen",
                Exit "west" "To the west is a lounge. " "lounge"]
               [mug]

kitchen = Room "You are in the kitchen."
               [Exit "south" "To the south is your bedroom. " "bedroom",
                Exit "west" "To the west is a hallway. " "hall"]
               [coffeepot]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit "east" "To the east is a kitchen. " "kitchen"]
            []
lounge = Room "You are in the lounge. The light switch is off"
            [Exit "east" "To the east is a bedroom. " "bedroom"]
            [laptop]

-- New data about the hall for when we open the door

openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit "east" "To the east is a kitchen. " "kitchen",
               Exit "out" "You can go outside. " "street"]

street = Room "You have made it out of the house. To finish the game you must bring your laptop to the lecture."
              [Exit "in" "You can go back inside if you like. " "hall"]
              []

gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street),
             ("lounge", lounge)]

initState :: GameData
initState = GameData "bedroom" gameworld [] False False False False

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))
