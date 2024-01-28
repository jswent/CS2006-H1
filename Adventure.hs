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
process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case actions cmd of
                            Just fn -> fn arg state
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case commands cmd of
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

{-- This is the game loop --}
repl :: GameData -> IO GameData
repl state | finished state = return state
repl state = do print state
                putStr "What now? "
                hFlush stdout
                cmd <- getLine
                let (state', msg) = process state (words cmd)
                putStrLn msg
                if (won state') then do putStrLn winmessage
                                        return state'
                               else repl state'

{-- Make the initial call to the game loop using the GameData object created by "initState" (from "World.hs") --}
main :: IO ()
main = do repl initState
          return ()
