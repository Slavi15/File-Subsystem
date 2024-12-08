module Main where

import Data.Maybe (maybeToList, listToMaybe)

import FileSystem ( FileSystem(..) )

import Command ( Command(..), MKCommands(Touch, MkDir) )
import Add (mkFile, mkDirectory)
import Parser (parseCommand)
import Printer (printFile, printDirectory)

eval :: String -> [FileSystem] -> Maybe [FileSystem]
eval input fs =
    case parseCommand input of
        Nothing -> Nothing
        Just (rest, curr) ->
            case curr of
                PWDCommand -> undefined
                CDCommand -> undefined
                CATCommand -> undefined
                DIRCommand command ->
                    case command of
                        Touch -> Just $ maybeToList (mkFile rest (listToMaybe fs))
                        MkDir -> Just $ maybeToList (mkDirectory rest (listToMaybe fs))
                RMCommand -> undefined
                QUITCommand -> undefined
                _ -> Nothing

repl :: [FileSystem] -> IO ()
repl fs = do
    putStr $ printDirectory fs ++ "> "
    input <- getLine
    case eval input fs of
        Nothing -> repl fs
        (Just fs') -> repl fs'

main :: IO ()
main = repl [fileSystem]

fileSystem :: FileSystem
fileSystem = MkDirectory "/" [MkDirectory "test" []]

-- >>> mkFile "/test file1 hello file1$ file2 hello file2$" (Just fileSystem)
-- Just (MkDirectory "/" [MkDirectory "test" [MkFile "file1" "hello file1",MkFile "file2" "hello file2"]])
