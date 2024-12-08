module Main where

import Data.Maybe (maybeToList, listToMaybe)

import FileSystem ( FileSystem(..) )

import Command ( Command(..), MKCommands(Touch, MkDir) )
import Add (mkFile, mkDirectory)
import Parser (parseCommand)
import Printer (printFile, printDirectory)

eval :: String -> Maybe FileSystem -> Maybe FileSystem
eval input fs =
    case parseCommand input of
        Nothing -> Nothing
        Just (rest, curr) ->
            case curr of
                PWDCommand -> undefined
                CDCommand -> undefined
                CATCommand -> undefined
                DIRCommand Touch -> mkFile rest fs
                DIRCommand MkDir -> mkDirectory rest fs
                RMCommand -> undefined
                QUITCommand -> undefined
                _ -> Nothing

repl :: Maybe FileSystem -> IO ()
repl fs = do
    putStr $ printDirectory (maybeToList fs) ++ "> "
    input <- getLine
    case eval input fs of
        Nothing -> repl fs
        (Just fs') -> repl (Just fs')

main :: IO ()
main = repl fileSystem

fileSystem :: Maybe FileSystem
fileSystem = Just (MkDirectory "/" [])

-- >>> mkFile "file1 hello file1$ file2 hello file2$" fileSystem
-- Just (MkDirectory "/" [MkFile "file1" "hello file1",MkFile "file2" "hello file2"])
