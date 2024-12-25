module Main where

import Data.Maybe (listToMaybe)
import Data.List (find)

import Core.FileSystem ( FileSystem(..) )
import Core.Command ( Command(..), MKCommands(Touch, MkDir), RMCommands (RM, RmDir) )
import Core.Eval ( Eval(..) )

import Add (mkFile, mkDirectory)
import Navigation (pwd, cd, ls)
import Cat (cat)
import Remove (rmFile, rmDirectory)
import Parser (parseCommand, wordParser)
import Show (showFile)
import Output (printDirectory, printFile)
import Utility (getName)

eval :: String -> [FileSystem] -> Eval
eval input fs =
    case parseCommand input of
        Nothing -> Continue $ Just fs
        Just (rest, curr) ->
            case curr of
                PWDCommand -> PWD
                CDCommand -> Continue $ cd ("/" ++ rest) fs
                LSCommand -> LS ("/" ++ rest)
                CATCommand -> Continue $ cat rest fs
                DIRCommand Touch -> Continue $ mkFile rest fs
                DIRCommand MkDir -> Continue $ mkDirectory rest fs
                RMCommand RM -> Continue $ rmFile rest fs
                RMCommand RmDir -> Continue $ rmDirectory rest fs
                SHOWCommand -> SHOW rest
                QUITCommand -> QUIT

repl :: [FileSystem] -> IO ()
repl fs = do
    putStr $ reverse (pwd fs) ++ "> "
    input <- getLine
    case eval input fs of
        Continue Nothing -> repl fs
        Continue (Just fs') -> repl fs'
        PWD -> do
                putStrLn $ pwd fs ++ "\n"
                repl fs
        LS path -> do
                putStrLn $ ls path (listToMaybe fs)
                repl fs
        SHOW fileName -> do
                putStrLn $ showFile fileName (listToMaybe fs)
                repl fs
        QUIT -> putStrLn "Exit ..."

main :: IO ()
main = repl [fileSystem]

fileSystem :: FileSystem
fileSystem =
    MkDirectory "/"
    [
        MkDirectory "scheme"
        [
            MkDirectory "test"
            [
                MkFile "haskell1" "hask1",
                MkFile "haskell2" "hask2",
                MkFile "haskell3" "hask3"
            ]
        ],
        MkDirectory "scheme2" [],
        MkFile "haskell" "",
        MkFile "haskell2" ""
    ]

-- >>> mkFile "file1 hello file1$ file2 hello file2$" fileSystem
-- Just (MkDirectory "/" [MkDirectory "scheme" [MkDirectory "test" [MkFile "haskell1" "hask1",MkFile "haskell2" "hask2",MkFile "haskell3" "hask3"]],MkDirectory "scheme2" [],MkFile "haskell" "",MkFile "haskell2" "",MkFile "file1" "hello file1",MkFile "file2" "hello file2",MkFile "" ""])

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
