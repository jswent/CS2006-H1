module Actions where

import Control.Monad
import Control.Monad.State

import Data.List
import Data.Maybe (listToMaybe)

import World


{-- Function that returns an Action based on user input. --}
actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just getAction
actions "put"     = Just putAction
actions "examine" = Just examine
actions "drink"   = Just drink
actions _         = Nothing


{-- Function that returns a Command based on user input. --}
commands :: String -> Maybe Command
commands "pour"      = Just pour
commands "open"      = Just open
commands "press"     = Just press
commands "quit"      = Just quit
commands "shower"    = Just shower
commands "inventory" = Just inv
commands "help"      = Just help
commands _           = Nothing


{-- 
    Take a String and convert it to an Argument for an Action, if applicable. 

    If the user's input_string cannot be converted into a Direction, then an attempt will
    be made to convert it into an Object. Regardless of whether the Object conversion should
    fail, its result will be returned.
--}
arguments :: String -> Maybe Argument
arguments "north"     = Just (DirArg North)
arguments "east"      = Just (DirArg East)
arguments "south"     = Just (DirArg South)
arguments "west"      = Just (DirArg West)
arguments "in"        = Just (DirArg In)
arguments "out"       = Just (DirArg Out)
arguments "mug"       = Just (ObjArg mug)
-- arguments "fullmug"   = Just (ObgArg fullmug)   <- (DO WE STILL NEED THIS?)
arguments "coffeepot" = Just (ObjArg coffeepot)
arguments "laptop"    = Just (ObjArg laptop)
arguments "beer"      = Just (ObjArg beer)
arguments _           = Nothing


{-- 
    Takes a Direction and the CURRENT room.

    Recurse through the "exit" values for the current room and search for an exit 
    in the same direction as the user wishes to move.

    If the room contains an exit which is in the specified direction, return this as a
    wrapped value. 
   
    If no exit exists from the current room in the user's chosen direction,
    the function will evaluate to Nothing.
   
    e.g. try these at the ghci prompt

    *Main> move "north" bedroom
    Just "kitchen"

    *Main> move "north" kitchen
    Nothing
--}
move :: Direction -> Room -> Maybe RoomID
move dir rm = fmap room . listToMaybe $ filter (\exit -> exit_dir exit == dir) (exits rm)

                                                 
-- WE NEED TO DECIDE BETWEEN THE ABOVE VERSION AND THE BELOW VERSION OF "move". WHICH IS MORE IDIOMATIC?
-- THE VERSION BELOW DOES USE FILTER, BUT AT THE COST OF READABILITY?
-- move :: Direction -> Room -> Maybe RoomID
-- move dir room | (length $ validExits) /= 0 = Just $ room $ head validExits
--               | otherwise                  = Nothing
--               where validExits = filter (\exit -> exit_dir exit == dir) (exits room)


{-- Return True if the object appears in the room. --}
objectHere :: WorldObject -> Room -> Bool
objectHere user_object room = any (\obj -> (obj_name obj) == (obj_name user_object)) (objects room)


{-- 
    Given an object id and a room description, return a new room description
    without that object. 
--}
removeObject :: WorldObject -> Room -> Room
removeObject user_object room = room { objects = filter ((/= obj_name user_object) . obj_name) (objects room)}


{--
    Given an object and a room description, return a new room description
    with that object added.
--}
addObject :: WorldObject -> Room -> Room
addObject new_object room | objectHere new_object room = room
                          | otherwise                  = let new_objects = (objects room) ++ [new_object]
                                                           in (room {objects = new_objects})
                                                           

{-- 
   Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') 
   
   --Rory - Head is safe to use here as we can assume object is present in list
--}
findObj :: ObjectType -> [WorldObject] -> WorldObject
findObj target_object objects = head $ filter (\obj -> obj_name obj == target_object) objects


{-- Use 'findObj' to find an object in a room description --}
objectData :: WorldObject -> Room -> WorldObject
objectData user_object rm = findObj (obj_name user_object) (objects rm)


{-- 
    Given a game state and a room id, replace the old room information with
    new data. If the room id does not already exist, add it.
--}
updateRoom :: RoomID -> Room -> State GameData ()
updateRoom room_id room_data = modify updateRoomData
  where
    updateRoomData game_data = game_data { world = map updateRoomInWorld (world game_data) }
    updateRoomInWorld tuple@(id, room) = if id == room_id then (room_id, room_data) else tuple

{-- Given a RoomID, find the Room object with the corresponding ID value and return it --}
getRoom :: RoomID -> State GameData Room
getRoom room_id = do
    game_data <- get
    let maybeRoom = lookup room_id (world game_data)
    case maybeRoom of
        Just room -> return room
        Nothing -> error "Room not found"  -- or handle the error appropriately

{-- 
    Given a game state and an object id, find the object in the current
    room and add it to the player's inventory 
    TODO: RE-WRITE WITH MAYBE   
--}
addInv :: WorldObject -> State GameData ()
addInv user_object = do
    state <- get
    currentRoom <- getRoom (location_id state)
    when (objectHere user_object currentRoom) $ do
        let newObject = objectData user_object currentRoom
        modify (\s -> s { inventory = newObject : inventory s })

{-- 
    Given a game state and an object id, remove the object from the
    inventory.
--}
removeInv :: WorldObject -> State GameData ()
removeInv user_object = modify removeInvFromState
  where
    removeInvFromState game_data =
        game_data { inventory = filter (\obj -> obj_name obj /= obj_name user_object) (inventory game_data) }


{-- Return True if the inventory in the game state contains the given object. --}
carrying :: WorldObject -> GameData -> Bool
carrying user_object game_data = 
    any (\obj -> obj_name obj == obj_name user_object) (inventory game_data)


{--
    Define the "go" action. Given a direction and a game state, update the game
    state with the new location. If there is no exit that way, report an error.
    Remember Actions return a 2-tuple of GameData and String. The String is
    a message reported to the player.

    e.g.
    *Main> go "north" initState
    (kitchen,"OK")
--}
go :: Action
go (DirArg direction) = do
    state <- get
    currentRoom <- getRoom (location_id state)
    let newRoomMaybe = move direction currentRoom
    case newRoomMaybe of
        Just newRoom -> do
            modify (\s -> s { location_id = newRoom })
            return "OK"
        Nothing -> return "No room in that direction."

{-- 
    Remove an item from the current room, and put it in the player's inventory.
    This should only work if the object is in the current room. Use 'objectHere'
    and 'removeObject' to remove the object, and 'updateRoom' to replace the
    room in the game state with the new room which doesn't contain the object.
--}
getAction :: Action
getAction (ObjArg user_object) = do
    state <- get
    currentRoom <- getRoom (location_id state)
    if objectHere user_object currentRoom then do
        let newRoom = removeObject user_object currentRoom
        addInv user_object                     -- Directly use addInv
        updateRoom (location_id state) newRoom  -- Directly use updateRoom
        return "Item picked up successfully"
    else
        return "Item not in room"

{-- 
    Remove an item from the player's inventory, and put it in the current room.
    Similar to 'getAction' but in reverse - find the object in the inventory, create
    a new room with the object in, update the game world with the new room.
--}
putAction :: Action
putAction (ObjArg user_object) = do
    state <- get
    if carrying user_object state then do
        currentRoom <- getRoom (location_id state)
        let newRoom = addObject user_object currentRoom
        updateRoom (location_id state) newRoom  -- Directly use updateRoom
        removeInv user_object                   -- Directly use removeInv
        return "Item put down successfully"
    else
        return "Item not in inventory"

{-- 
    Don't update the state, just return a message giving the full description
    of the object. As long as it's either in the room or the player's 
    inventory!
--}
examine :: Action
examine (ObjArg user_object) = do
    state <- get
    currentRoom <- getRoom (location_id state)
    if objectHere user_object currentRoom || carrying user_object state then
        let object = if objectHere user_object currentRoom
                     then objectData user_object currentRoom
                     else findObj (obj_name user_object) (inventory state)
        in return $ obj_longname object ++ ": " ++ obj_desc object
    else
        return "The object is neither in the room nor in your inventory."

{-- 
    Pour the coffee. Obviously, this should only work if the player is carrying
    both the pot and the mug. This should update the status of the "mug"
    object in the player's inventory to be a new object, a "full mug".
--}
pour :: Command
pour = do
    state <- get
    if carrying coffeepot state && carrying mug state && not (poured state) then do
        let newInventory = fullmug : filter (\obj -> obj_name obj /= Mug) (inventory state)
        put $ state { inventory = newInventory, poured = True }
        return "Coffee mug is now full and ready to drink"
    else if carrying coffeepot state && carrying mug state then
        return "Coffee mug is already full and ready to drink"
    else
        return "Cannot pour coffee until you have both the coffee pot and a mug in your inventory"

{-- 
    Drink the coffee. This should only work if the player has a full coffee 
    mug! Doing this is required to be allowed to open the door. Once it is
    done, also update the 'caffeinated' flag in the game state.

    Also, put the empty coffee mug back in the inventory!
--}
drink :: Action
drink (ObjArg object) = do
    state <- get
    if carrying mug state && (poured state) && object == mug then do
        let newInventory = mug : filter (\obj -> obj_name obj /= Mug) (inventory state)
        put $ state { inventory = newInventory, caffeinated = True, poured = False, drunk = False }
        return "Coffee has been drunk and you are now caffeinated"
    else if carrying beer state && object == beer then do
        let newInventory = filter(\obj -> obj_name obj /= Beer) (inventory state)
        put $ state {inventory = newInventory, drunk =  True}
        return "Beer has been drunk and you are now pissed. You must have a coffee again to sober up"
    else
        return "To drink you must have a full mug of coffee or a beer in your inventory"

{-- 
    Open the door. Only allowed if the player has had coffee! 
    This should change the description of the hall to say that the door is open,
    and add an exit out to the street.

    Use 'updateRoom' once you have made a new description. You can use 
    'openedhall' and 'openedexits' from World.hs for this.
--}
open :: Command
open = do
    state <- get
    if caffeinated state && (location_id state) == Hall then do
        let newHall = hall { room_desc = openedhall, exits = openedexits }
        updateRoom Hall newHall
        return "Door has been opened to the street!"
    else
        return "You are too sleepy. To open the door you must have drunk a mug of coffee."

{--
    Press the light switch. Only allowed when player is in the lounge.
    This will allow players to see where they are going.
--}
press :: Command
press = do
    state <- get
    if (location_id state) == Lounge then do
        put $ state { light = True }
        return "Light is switched on."
    else
        return "To turn on the light you must be in the lounge."

shower :: Command
shower = do
    state <- get
    let newBathroom = bathroom {
            room_desc = bathroomShoweredDesc
    }
    if (location_id state) == Bathroom && not (showered state) then do
        put $ state { showered = True }
        updateRoom Bathroom newBathroom
        return "You took a shower"
    else if (location_id state) == Bathroom && (showered state) then do
        return "You have already showered this morning"
    else return "To take a shower you must be in your bathroom"


{-- Don't update the game state, just list what the player is carrying. --}
inv :: Command
inv = do
    state <- get
    return $ showInv (inventory state)
    where showInv [] = "You aren't carrying anything"
          showInv xs = "You are carrying:\n" ++ intercalate "\n" (map obj_longname xs) -- more idiomatic
          {-- This is the only way I could figure out how to use foldr without printing an additional newline, pretty ugly. --}
          -- showInv xs = "You are carrying:\n" ++ foldr appendItem "" xs
          -- appendItem item acc
          --   | null acc  = obj_longname item
          --   | otherwise = obj_longname item ++ "\n" ++ acc

{-- Display a help message to the user, taking into account the current items they have and the tasks they need to complete --}
help :: Command
help = do
    state <- get
    return helpMessage

helpMessage :: ReturnValue
helpMessage = "----- Haskell-P1 -----\n" ++
              "ACTIONS:\n" ++
              "  go      [Direction]\n" ++
              "  get     [Object]\n" ++
              "  put     [Object]\n" ++
              "  examine [Object]\n" ++
              "  drink   [Object]\n" ++
              "\n" ++
              "COMMANDS:\n" ++
              "  pour\n" ++
              "  open\n" ++
              "  press\n" ++
              "  inventory\n" ++
              "  help\n" ++
              "  quit\n" ++
              "\n" ++
              "OBJECTS:\n" ++
              "  mug\n" ++
              "\n" ++
              "DIRECTIONS:\n" ++
              "  north\n" ++
              "  east\n" ++
              "  south\n" ++
              "  west\n" ++
              "  in\n" ++
              "  out"


{-- End the game loop and display a message to the player. --}
quit :: Command
quit = do
    modify (\s -> s { finished = True })
    return "Bye bye"
