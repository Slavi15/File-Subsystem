module Main where

import Data.Maybe (maybeToList, listToMaybe)

import Core.FileSystem ( FileSystem(..) )
import Core.Command ( Command(..), MKCommands(Touch, MkDir), RMCommands (RM, RmDir) )
import Core.Eval ( Eval(..) )

import Add (mkFile, mkDirectory)
import Navigation (pwd, cd, ls)
import Remove (rmFile, rmDirectory)
import Parser (parseCommand, wordParser)
import Output (printDirectory)
import Utility (getName)

eval :: String -> Maybe FileSystem -> Eval
eval input fs =
    case parseCommand input of
        Nothing -> Continue Nothing
        Just (rest, curr) ->
            case curr of
                PWDCommand -> PWD
                CDCommand -> Continue $ cd (pwd fs fileSystem ++ rest) (maybeToList fileSystem)
                LSCommand -> LS ("/" ++ rest)
                CATCommand -> undefined
                DIRCommand Touch -> Continue $ mkFile rest fs
                DIRCommand MkDir -> Continue $ mkDirectory rest fs
                RMCommand RM -> Continue $ rmFile rest fs
                RMCommand RmDir -> Continue $ rmDirectory rest fs
                QUITCommand -> Quit

repl :: Maybe FileSystem -> IO ()
repl fs = do
    putStr $ pwd fs fileSystem ++ "> "
    input <- getLine
    case eval input fs of
        Continue Nothing -> repl fs
        (Continue fs') -> repl fs'
        PWD -> do
                putStrLn $ pwd fs fileSystem ++ "\n"
                repl fs
        LS path -> do
                putStrLn $ ls path fs
                repl fs
        Quit -> putStrLn "Exit ..."

main :: IO ()
main = repl fileSystem

fileSystem :: Maybe FileSystem
fileSystem = Just (MkDirectory "/" [MkDirectory "scheme" [MkDirectory "test" [MkFile "haskell" "", MkFile "haskell" "", MkFile "haskell" ""]], MkFile "haskell" ""])

-- >>> mkFile "file1 hello file1~ file2 hello file2~" fileSystem
-- Just (MkDirectory "/" [MkFile "file" "content",MkFile "file1" "hello file1",MkFile "file2" "hello file2"])

-- >>> mkDirectory "dir1 dir2 dir3" fileSystem
-- Just (MkDirectory "/" [MkFile "file" "content",MkDirectory "dir1" [],MkDirectory "dir2" [],MkDirectory "dir3" []])

-- >>> rmFile "file file2" fileSystem
-- Just (MkDirectory "/" [MkDirectory "dir" []])

-- >>> rmDirectory "dir" fileSystem
-- Just (MkDirectory "/" [MkFile "file" "content",MkFile "file2" "content2"])

-- >>> ls "" (cd "/.." [(MkDirectory "/" [MkDirectory "scheme" []])])
-- "Invalid directory!\n"

-- >>> ls "" fileSystem
-- "Directory: scheme\nFile: haskell\n"
