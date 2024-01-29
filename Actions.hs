module Actions where

import World

{-- Function that returns an Action based on user input --}
actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions _         = Nothing

{-- Function that returns a Command based on user input --}
commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

{-- 
   An alternative to "head", which will return the first element in 
   a list if applicable, but instead of throwing an error when an 
      empty list is passed in Nothing will be returned
--}
safeHead :: [a] -> Maybe a
safeHead []       = Nothing
safeHead (x : xs) = Just x

{-- Take a String representing a direction and return the opposite direction --}
opposite :: Direction -> Maybe Direction
opposite North = Just South
opposite East  = Just West
opposite South = Just North
opposite West  = Just East
-- opposite _      = Nothing

{-- 
   Takes a direction and the current room.

   Recurse through the "exit" values for the current room and search for an exit 
   in the same direction as the user wishes to move.

   If the room contains an exit which is in the specified direction, return this as a
   wrapped value. 
   
   If no exit exists from the current room in the user's chosen direction,
   the function will evaluate to Nothing.
 --}
move :: String -> Room -> Maybe String
move dir (Room _ [] _) = Nothing
move dir (Room a (exit:exits) b)
   | exit_dir exit == dir = Just $ room exit
   | otherwise = move dir (Room a exits b)

{- Return True if the object appears in the room. -}

objectHere :: String -> Room -> Bool
objectHere o (Room _ _ []) = False
objectHere o (Room a b (object:objects)) 
   | obj_name object == o = True
   | otherwise = objectHere o (Room a b objects)

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: String -> Room -> Room
removeObject o rm = 
   let 
      objs = objects rm
      new_objs = filter (\obj -> obj_name obj /= o) objs
   in (
        rm {objects = new_objs})

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm
   | objectHere (obj_name o) rm = rm
   | otherwise = let new_objs = (objects rm) ++ [o]
      in (rm {objects = new_objs})

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

--Rory - Head is safe to use here as we can assume object is present in list
findObj :: String -> [Object] -> Object
findObj o os = head $ filter (\obj -> obj_name obj == o) os

{- Use 'findObj' to find an object in a room description -}

objectData :: String -> Room -> Object
objectData o rm = findObj o (objects rm)

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata
   | length roomArr == 0 = gd {world = (rmid, rmdata) : world gd}
   | otherwise = 
      let newWorld = map (\tuple -> if (fst tuple) == rmid then (rmid, rmdata) else tuple) (world gd)
      in (
         gd {
            world = newWorld
         }
      )
   where
      roomArr = filter (\roomTuple -> fst roomTuple == rmid) (world gd)


getRoom :: String -> GameData -> Room
getRoom rmid gd = 
   let 
      rooms = world gd
   in 
      snd $ head $ filter (\room -> fst room == rmid) rooms

{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> String -> GameData
addInv gd obj = 
   let
      room = getRoom (location_id gd) gd
      object | objectHere obj room = [objectData obj room]
             | otherwise           = []
   in (
      gd {
         inventory = inventory gd ++ object
      }
   )

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> String -> GameData
removeInv gd obj = 
   gd {
      inventory = filter (\object -> obj_name object /= obj) (inventory gd)
   }

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> String -> Bool
carrying gd obj = any (\object -> obj_name object == obj) (inventory gd)

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Action
go dir state =
   if newRmMybeStr == Nothing then (state, "No room in that direction.")
   else 
      (newState, "OK")
   where
      currentRm = getRoom (location_id state) state
      newRmMybeStr = move dir currentRm
      newRmStr = case newRmMybeStr of
         Nothing  -> ""
         Just val -> val
      newState = state {
         location_id = newRmStr
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

get :: Action
get obj state
   | objectHere obj room = (newState, "Item picked up successfully")
   | otherwise = (state, "Item not in room")
   where 
      room = getRoom (location_id state) state
      newRoom = removeObject obj room
      newState = updateRoom (addInv state obj) (location_id state) newRoom

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: Action
put obj state 
   | carrying state obj = (newState, "Item put down successfully")
   | otherwise = (state, "Item not in inventory")
   where 
      room = getRoom (location_id state) state
      object = findObj obj (inventory state)
      newRoom = addObject object room
      newState = updateRoom (removeInv state obj) (location_id state) newRoom

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj state 
   | objectHere obj rm || carrying state obj =  
      (state, obj_longname object ++ ": " ++ obj_desc object)
   where
      rm = getRoom (location_id state) state
      object = if objectHere obj rm 
               then objectData obj rm
               else findObj obj (inventory state)


{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state
   | carrying state "coffee" && carrying state "mug" && not (poured state)= (newState, "Coffee mug is now full and ready to drink")
   | carrying state "coffee" && carrying state "mug" = (state, "Coffee mug is already full and ready to drink")
   | otherwise = (state, "Cannot pour coffee until you have both the coffee pot and a mug in your inventory")
   where
      newInventory = fullmug : (filter (\object -> obj_name object /= "mug") (inventory state))
      newState = state { 
         inventory = newInventory,
         poured = True
      }

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state
   | carrying state "mug" && (poured state) = (newState, "Coffee has been drunk and you are now caffeinated")
   | otherwise = (state, "To drink the coffee you must have a full mug of coffee in your inventory")
   where
      newInventory = mug : (filter (\object -> obj_name object /= "mug") (inventory state))
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

open :: Action
open obj state --must be in hall
   | caffeinated state && (location_id state) == "hall" = (newState, "Door has been opened to the street!")
   | otherwise = (state, "You are too sleepy. To open the door you must have drunk a mug of coffee.")
   where
      newHall = hall {
         room_desc = openedhall,
         exits = openedexits }
      newState = (updateRoom state "hall" newHall)
      


{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")

