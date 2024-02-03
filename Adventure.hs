module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

{- 
   Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. 

   This is called once input has been received by "repl" so that the
   current state of the game can be updated accordingly
--}
process :: GameData -> [String] -> (GameData, ReturnValue)
process state [cmd,arg] = case actions cmd of  -- Check for action validity
                            Just fn -> case arguments arg of
                                        Just a -> fn a state
                                        Nothing -> (state, "I don't understand")
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case commands cmd of -- Check for command validity
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _         = (state, "I don't understand")

{-- This is the game loop --}
repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do print state  -- An IO action to display the contents of "state" on stdout
                putStr "What now? "
                hFlush stdout  -- Ensure the message has been printed to stdout
                cmd <- getLine  -- Get the user's input from stdin
                let (state', msg) = process state (words cmd)
                putStrLn msg
                if (won state') then do putStrLn winmessage  -- Check if the user has won
                                        return state'
                               else repl state' -- If the user has not won yet, continue the loop with the updated State

{-- Make the initial call to the game loop using the GameData object created by "initState" (from "World.hs") --}
main :: IO ()
main = do repl initState
          return ()
