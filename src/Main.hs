module Main where

import Data.Maybe (maybeToList, listToMaybe)

import Data.FileSystem ( FileSystem(..) )
import Data.Command ( Command(..), MKCommands(Touch, MkDir), RMCommands (RM, RmDir) )
import Data.Eval ( Eval(..) )

import Add (mkFile, mkDirectory)
import Remove (rmFile, rmDirectory)
import Parser (parseCommand)
import Printer (printDirectory)

eval :: String -> Maybe FileSystem -> Eval
eval input fs =
    case parseCommand input of
        Nothing -> Continue Nothing
        Just (rest, curr) ->
            case curr of
                PWDCommand -> undefined
                CDCommand -> undefined
                CATCommand -> undefined
                DIRCommand Touch -> Continue $ mkFile rest fs
                DIRCommand MkDir -> Continue $ mkDirectory rest fs
                RMCommand RM -> Continue $ rmFile rest fs
                RMCommand RmDir -> Continue $ rmDirectory rest fs
                QUITCommand -> Quit
                _ -> Continue Nothing

repl :: Maybe FileSystem -> IO ()
repl fs = do
    putStr $ printDirectory (maybeToList fs) ++ "> "
    input <- getLine
    case eval input fs of
        Continue Nothing -> repl fs
        (Continue fs') -> repl fs'
        Quit -> putStrLn "Exit ..."

main :: IO ()
main = repl fileSystem

fileSystem :: Maybe FileSystem
fileSystem = Just (MkDirectory "/" [MkFile "file" "content", MkDirectory "dir" [], MkFile "file2" "content2"])

-- >>> mkFile "file1 hello file1~ file2 hello file2~" fileSystem
-- Just (MkDirectory "/" [MkFile "file" "content",MkFile "file1" "hello file1",MkFile "file2" "hello file2"])

-- >>> mkDirectory "dir1 dir2 dir3" fileSystem
-- Just (MkDirectory "/" [MkFile "file" "content",MkDirectory "dir1" [],MkDirectory "dir2" [],MkDirectory "dir3" []])

-- >>> rmFile "file file2" fileSystem
-- Just (MkDirectory "/" [MkDirectory "dir" []])

-- >>> rmDirectory "dir" fileSystem
-- Just (MkDirectory "/" [MkFile "file" "content",MkFile "file2" "content2"])
