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


{-Checks if an input begins with the word save-}
isSaveCommand :: String -> Bool
isSaveCommand input = case input of
    's':'a':'v':'e':' ':_ -> True
    _ -> False
{-Checks if an input begins with the word load-}
isLoadCommand :: String -> Bool
isLoadCommand input = case input of
    'l':'o':'a':'d':' ':_ -> True
    _ -> False

-- Convert a ByteString to a String
byteStringToString :: B.ByteString -> String
byteStringToString = B.unpack

-- Convert a String to a ByteString 
stringToByteString :: String -> B.ByteString
stringToByteString = B.pack

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



main :: IO ()
main = runInputT defaultSettings (repl initState) >> return ()

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

getFilePath :: String -> String
getFilePath xs
  | length xs > 5 = drop 5 xs
  | otherwise = "file" --Default FilePath