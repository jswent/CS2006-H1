module Main where

import World
import Actions

import Control.Monad
import System.IO
import System.Exit
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson

import System.Console.Haskeline

winmessage = "Congratulations, you have made it out of the house.\n" ++
             "Now go to your lectures..."


{-- Checks if an input begins with the word "save". --}
isSaveCommand :: String -> Bool
isSaveCommand input = case input of
    's':'a':'v':'e':_ -> True
    _ -> False


{-- Checks if an input begins with the word "load". --}
isLoadCommand :: String -> Bool
isLoadCommand input = case input of
    'l':'o':'a':'d':' ':_ -> True
    _ -> False


{-- Convert a ByteString to a String --}
byteStringToString :: B.ByteString -> String
byteStringToString = B.unpack


{-- Convert a String to a ByteString --}
stringToByteString :: String -> B.ByteString
stringToByteString = B.pack


{--
    Given a game state, and user input (as a list of words) return a 
    new game state and a message for the user.

    This is called once input has been received by "repl" so that the
    current state of the game can be updated accordingly.
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


{-- This is the game loop. --}
repl :: GameData -> InputT IO GameData
repl state | finished state = return state
repl state = do
    outputStrLn ""
    outputStrLn $ show state ++ "\n"
    outputStr "What now? "
    mcmd <- getInputLine ""
    case mcmd of
        Nothing -> return state  -- Handle end-of-input (e.g., EOF/Ctrl-D)
        Just cmd -> do
            if isSaveCommand cmd then do
                liftIO $ writeFile (getFilePath cmd) (byteStringToString (encode state))
                outputStrLn "Game saved successfully"
                repl state
            else if isLoadCommand cmd then do
                newState <- handleLoad cmd
                outputStrLn "Game Loaded successfully"
                repl newState
            else do
                let (state', msg) = process state (words cmd)
                outputStrLn msg
                if won state'
                    then do outputStrLn winmessage
                            return state'
                    else repl state'


{-- Make the initial call to the game loop using the GameData object created by "initState" (from "World.hs") --}
main :: IO ()
main = runInputT defaultSettings (repl initState) >> return ()


{-- INSERT HIGHLY INFORMATIVE COMMENT HERE --}
handleLoad :: String -> InputT IO GameData
handleLoad str =
  do
    let filePath = getFilePath str
    -- Read the contents of the file
    strState <- liftIO $ readFile filePath
    -- Convert the file contents to a ByteString
    let newStateBString = stringToByteString strState
    -- Decode the ByteString into a GameData value
    let newState = case decode newStateBString of
          Just gd -> gd
          Nothing -> error "Failed to decode GameData"
    return newState


{-- INSERT HIGHLY INFORMATIVE COMMENT HERE --}
getFilePath :: String -> String
getFilePath xs
  | length xs > 5 = drop 5 xs
  | otherwise     = "defaultfile.json" --Default FilePath