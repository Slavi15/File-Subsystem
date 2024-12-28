module Concat where

import Control.Applicative (Alternative (empty, (<|>)))
import Core.FileSystem ( FileSystem(..) )

import Data.List (find)

import Add (mkFile)
import Interact (readFileContent)
import Parser (wordParser)

catTwoFiles :: String -> FileSystem -> FileSystem -> Maybe FileSystem
catTwoFiles newFileName (MkFile _ cnt) (MkFile _ cnt') = Just $ MkFile newFileName (cnt ++ cnt')
catTwoFiles _ _ _ = Nothing

concatFiles :: String -> FileSystem -> FileSystem -> [FileSystem] -> IO (Maybe [FileSystem])
concatFiles outputFileName resultFile currFile fs = 
    case (outputFileName, currFile) of
        ("", MkFile _ "") -> do
            content <- readFileContent
            putStrLn content
            pure $ Just fs
        ("", MkFile _ content) -> do
            putStrLn content
            pure $ Just fs
        (outputFileName, MkFile _ "") -> do
            content <- readFileContent
            case catTwoFiles outputFileName resultFile (MkFile outputFileName content) of
                Nothing -> pure $ Just fs
                Just (MkFile newName newContent) ->
                    pure $ mkFile (newName ++ " " ++ newContent ++ "$") fs <|> Just fs
        (outputFileName, currFile) ->
            case catTwoFiles outputFileName (MkFile "" "") currFile of
            Nothing -> pure $ Just fs
            Just (MkFile newName content) ->
                pure $ mkFile (newName ++ " " ++ content ++ "$") fs <|> Just fs

cat :: String -> [FileSystem] -> IO (Maybe [FileSystem])
cat = cat' (MkFile "" "")
    where
        cat' :: FileSystem -> String -> [FileSystem] -> IO (Maybe [FileSystem])
        cat' currFile input fs@((MkDirectory _ contents) : _) =
            case wordParser input of
                Just ("", "") -> pure $ Just fs
                Just (outputFileName, ">") ->
                    concatFiles outputFileName (MkFile "" "") currFile fs
                Just (rest, curr) ->
                    case find (\(MkFile fileName _) -> fileName == curr) contents of
                        Nothing -> cat' currFile rest fs
                        Just file ->
                            case catTwoFiles "" currFile file of
                                Nothing -> cat' currFile rest fs
                                Just result -> cat' result rest fs
                Nothing -> pure $ Just fs