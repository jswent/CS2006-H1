module Actions where

import World

{-- Function that returns an Action based on user input --}
actions :: String -> Maybe Action
actions "go"      = Just Go
actions "get"     = Just Get
actions "drop"    = Just Put
actions "pour"    = Just Pour
actions "examine" = Just Examine
actions "drink"   = Just Drink
actions "open"    = Just Open
actions _         = Nothing

{-- Function that returns a Command based on user input --}
commands :: String -> Maybe Command
commands "quit"      = Just Quit
commands "inventory" = Just Inventory
commands _           = Nothing

{-- Take a String and convert it to the associated Direction type, if applicable --}
directions :: String -> Maybe Direction
directions "north" = Just North
directions "east"  = Just East
directions "south" = Just South
directions "west"  = Just West
directions "in"    = Just In
directions "out"   = Just Out
directions _       = Nothing

{--
   Take a Direction and return the opposite direction.
   The catch-all branch is not needed here, as any other input would create
      a compiler error.   
--}
opposite :: Direction -> Maybe Direction
opposite North   = Just South
opposite East    = Just West
opposite South   = Just North
opposite West    = Just East
opposite Out     = Just In
opposite In      = Just Out

{-- 
   An alternative to "head", which will return the first element in 
   a list if applicable, but instead of throwing an error when an 
      empty list is passed in Nothing will be returned
--}
safeHead :: [a] -> Maybe a
safeHead []       = Nothing
safeHead (x : xs) = Just x

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

{-- 
   Takes a Direction and the CURRENT room.

   Recurse through the "exit" values for the current room and search for an exit 
   in the same direction as the user wishes to move.

   If the room contains an exit which is in the specified direction, return this as a
   wrapped value. 
   
   If no exit exists from the current room in the user's chosen direction,
   the function will evaluate to Nothing.
--}
move :: Direction -> Room -> Maybe RoomID
move direction (Room _         _ []           _) = Nothing
move direction (Room room_type a (exit:exits) b) | exit_dir exit == direction = Just $ room exit
                                                 | otherwise                  = move direction (Room room_type a exits b)


{- Return True if the object appears in the room. -}
objectHere :: Object -> Room -> Bool
objectHere o (Room _         _ _ []              ) = False
objectHere o (Room room_type a b (object:objects)) | obj_name object == obj_name o = True
                                                   | otherwise                     = objectHere o (Room room_type a b objects)


{- Given an object id and a room description, return a new room description
   without that object -}
removeObject :: Object -> Room -> Room
removeObject user_object room = 
   let 
      objs = objects room
      new_objs = filter (\obj -> obj_name obj /= obj_name user_object) objs
   in (
        room {objects = new_objs})


{- Given an object and a room description, return a new room description
   with that object added -}
addObject :: Object -> Room -> Room
addObject new_object room
   | objectHere new_object room = room
   | otherwise                  = let new_objects = (objects room) ++ [new_object]
                                     in (room {objects = new_objects})

{-- 
   Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') 
   
   --Rory - Head is safe to use here as we can assume object is present in list
--}
findObj :: ObjectType -> [Object] -> Object
findObj target_object objects = head $ filter (\obj -> obj_name obj == target_object) objects


{- Use 'findObj' to find an object in a room description -}
objectData :: Object -> Room -> Object
objectData user_object rm = findObj (obj_name user_object) (objects rm)


{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}
updateRoom :: GameData -> RoomID -> Room -> GameData
updateRoom game_data room_id room_data
   | length roomArr == 0 = game_data { world = (room_id, room_data) : world game_data }
   | otherwise           = let newWorld = map (\tuple -> if (fst tuple) == room_id then (room_id, room_data) else tuple) (world game_data)
                           in ( game_data { world = newWorld } )
   where
      roomArr = filter (\roomTuple -> fst roomTuple == room_id) (world game_data)


{-- Given a RoomID, find the Room object with the corresponding ID value and return it --}
getRoom :: RoomID -> GameData -> Room
getRoom room_id game_data = let rooms = world game_data
                              in snd $ head $ filter (\room -> fst room == room_id) rooms


{-- 
   Given a game state and an object id, find the object in the current
   room and add it to the player's inventory 
   RE-WRITE WITH MAYBE   
--}
-- NOT REFACTORED --
addInv :: GameData -> Object -> GameData
addInv game_data user_object = let room = getRoom (location_id game_data) game_data
                                   object | objectHere user_object room = [objectData user_object room]
                                          | otherwise              = []
                              in ( game_data { inventory = inventory game_data ++ object } )
   
-- addInv :: GameData -> Object -> GameData
-- addInv game_data user_object = let room = getRoom (location_id game_data) game_data
--                                    object | objectHere object room = objectData object room
--                                  in ( game_data { inventory = inventory game_data ++ object } )


{- 
   Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory.
-}
-- NOT REFACTORED --
removeInv :: GameData -> Object -> GameData
removeInv game_data user_object = 
   game_data { inventory = filter (\obj -> obj_name obj /= obj_name user_object) (inventory game_data) }


{- Does the inventory in the game state contain the given object? -}
-- REFACTOR COMPLETE --
carrying :: GameData -> Object -> Bool
carrying game_data user_object = any (\obj -> obj_name obj == obj_name user_object) (inventory game_data)


{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}
-- REFACTOR NOT COMPLETE --
go :: Direction -> GameData -> (GameData, ReturnValue)
go direction state | (newRoomMaybeStr == Nothing) = (state, "No room in that direction.")
                   | otherwise                    = (newState, "OK")
                  where
                     currentRoom = getRoom (location_id state) state
                     newRoomMaybeStr = move direction currentRoom
                     newRoomStr = case newRoomMaybeStr of
                        Just value -> value -- If a value was 
                        Nothing    -> room_name currentRoom
                     newState = state {
                        location_id = newRoomStr
                     }


{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}
-- REFACTOR COMPLETE --
get :: Object -> GameData -> (GameData, ReturnValue)
get object state
   | objectHere object room = (newState, "Item picked up successfully")
   | otherwise = (state, "Item not in room")
   where 
      room = getRoom (location_id state) state
      newRoom = removeObject object room
      newState = updateRoom (addInv state object) (location_id state) newRoom

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: Object -> GameData -> (GameData, ReturnValue)
put user_object state 
   | carrying state user_object = (newState, "Item put down successfully")
   | otherwise          = (state, "Item not in inventory")
   where 
      room = getRoom (location_id state) state
      object = findObj (obj_name user_object) (inventory state)
      newRoom = addObject object room
      newState = updateRoom (removeInv state user_object) (location_id state) newRoom

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Object -> GameData -> (GameData, ReturnValue)
examine user_object state 
   | objectHere user_object rm || carrying state user_object =  
      (state, obj_longname object ++ ": " ++ obj_desc object)
   where
      rm = getRoom (location_id state) state
      object = if objectHere user_object rm 
               then objectData user_object rm
               else findObj (obj_name user_object) (inventory state)


{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Object -> GameData -> (GameData, ReturnValue)
pour object state
   | carrying state coffeepot && carrying state mug && not (poured state) = (newState, "Coffee mug is now full and ready to drink")
   | carrying state coffeepot && carrying state mug = (state, "Coffee mug is already full and ready to drink")
   | otherwise = (state, "Cannot pour coffee until you have both the coffee pot and a mug in your inventory")
   where
      newInventory = fullmug : (filter (\obj -> obj_name obj /= Mug) (inventory state)) -- Check that fullmug wasn't mug
      newState = state { 
         inventory = newInventory,
         poured = True
      }

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Object -> GameData -> (GameData, ReturnValue)
drink object state
   | carrying state mug && (poured state)   = (newState, "Coffee has been drunk and you are now caffeinated")
   | otherwise                              = (state, "To drink the coffee you must have a full mug of coffee in your inventory")
   where
      newInventory = mug : (filter (\obj -> obj_name obj /= Mug) (inventory state))
      newState = state { 
         inventory = newInventory,
         caffeinated = True,
         poured = False
      }


{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Object -> GameData -> (GameData, ReturnValue)
open obj state --must be in hall
   | caffeinated state && (location_id state) == Hall = (newState, "Door has been opened to the street!")
   | otherwise                                        = (state, "You are too sleepy. To open the door you must have drunk a mug of coffee.")
   where
      newHall = hall {  room_desc = openedhall,
                        exits = openedexits }
      newState = (updateRoom state Hall newHall)
      


{- Don't update the game state, just list what the player is carrying -}

inv :: GameData -> (GameData, ReturnValue)
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: GameData -> (GameData, ReturnValue)
quit state = (state { finished = True }, "Bye bye")

