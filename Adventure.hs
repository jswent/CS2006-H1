module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit

import System.Console.Haskeline

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."

{- Given a game state, and user input (as a list of words) return a 
   new game state and a message for the user. -}

process :: GameData -> [String] -> (GameData, String)
process state [cmd,arg] = case actions cmd of
                            Just fn -> fn arg state
                            Nothing -> (state, "I don't understand")
process state [cmd]     = case commands cmd of
                            Just fn -> fn state
                            Nothing -> (state, "I don't understand")
process state _ = (state, "I don't understand")

repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do
    outputStrLn $ show state
    outputStr "What now? "
    mcmd <- getInputLine ""
    case mcmd of
      Nothing -> return state  -- Handle end-of-input (e.g., EOF/Ctrl-D)
      Just cmd -> do
        let (state', msg) = process state (words cmd)
        outputStrLn msg
        if won state'
          then do outputStrLn winmessage
                  return state'
          else repl state'

main :: IO ()
main = runInputT defaultSettings (repl initState) >> return ()
