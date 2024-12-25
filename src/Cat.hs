module Cat where

import Control.Applicative (Alternative ((<|>)))
import Core.FileSystem ( FileSystem(..) )

import Data.List (find)

import Add (mkFile)
import Parser (wordParser)

catFiles :: String -> FileSystem -> FileSystem -> Maybe FileSystem
catFiles newFileName (MkFile _ cnt) (MkFile _ cnt') = Just $ MkFile newFileName (cnt ++ cnt')
catFiles _ _ _ = Nothing

cat :: String -> [FileSystem] -> Maybe [FileSystem]
cat = cat' (MkFile "" "")
    where
        cat' :: FileSystem -> String -> [FileSystem] -> Maybe [FileSystem]
        cat' currFile input fs@((MkDirectory _ contents) : _) =
            case wordParser input of
                Just ("", "") -> Just fs
                Just (outputFileName, ">") ->
                    case catFiles outputFileName (MkFile "" "") currFile of
                        Nothing -> Just fs
                        Just (MkFile newName content) ->
                            mkFile (newName ++ " " ++ content ++ "$") fs <|> Just fs
                Just (rest, curr) ->
                    case find (\(MkFile fileName _) -> fileName == curr) contents of
                        Nothing -> cat' currFile rest fs
                        Just file ->
                            case catFiles "" currFile file of
                                Nothing -> cat' currFile rest fs
                                Just result -> cat' result rest fs
                Nothing -> Just fs