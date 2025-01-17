{-# LANGUAGE LambdaCase #-}
module Main where

import Core.FileSystem ( FileSystem(..) )
import Core.Command ( Command(..), MKCommands(Touch, MkDir) )
import Core.Eval ( Eval(..) )

import Add (mkFile, mkDirectory)
import Navigation (pwd, cd, ls)
import Concat (cat)
import Remove (rmFiles)
import Parser (parseCommand, wordParser)
import Show (showFile)

eval :: String -> [FileSystem] -> IO Eval
eval input fs =
    case parseCommand input of
        Nothing -> pure $ Continue $ Just fs
        Just (rest, curr) ->
            case curr of
                PWDCommand -> pure PWD
                CDCommand -> pure $ Continue $ cd rest fs
                LSCommand -> pure $ LS rest
                CATCommand -> cat rest fs >>= \result -> pure $ Continue result
                DIRCommand Touch -> pure $ Continue $ mkFile rest fs
                DIRCommand MkDir -> pure $ Continue $ mkDirectory rest fs
                RMCommand -> pure $ Continue $ rmFiles rest fs
                SHOWCommand -> pure $ SHOW rest
                QUITCommand -> pure QUIT

repl :: [FileSystem] -> IO ()
repl fs = do
    putStr $ pwd fs ++ "> "
    input <- getLine
    eval input fs >>= \case
        Continue Nothing -> repl fs
        Continue (Just fs') -> repl fs'
        PWD-> do
            putStrLn $ pwd fs ++ "\n"
            repl fs
        LS path -> do
            putStrLn $ ls path fs
            repl fs
        SHOW fileName -> do
            putStrLn $ showFile fileName fs
            repl fs
        QUIT -> putStrLn "Exit ..."

main :: IO ()
main = repl [fileSystem]

fileSystem :: FileSystem
fileSystem = MkDirectory "/" []

-- fileSystem =
--     MkDirectory "/"
--     [
--         MkDirectory "scheme"
--         [
--             MkDirectory "test"
--             [
--                 MkFile "haskell1" "hask1",
--                 MkFile "haskell2" "hask2",
--                 MkFile "haskell3" "hask3"
--             ]
--         ],
--         MkDirectory "scheme2" [],
--         MkFile "haskell" "",
--         MkFile "haskell2" ""
--     ]
