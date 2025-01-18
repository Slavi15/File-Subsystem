module Add where

import Data.List (nub)

import Core.FileSystem ( FileSystem(..) )

import Parser (wordParser, getNextDirectory, eofParser)
import Utility (isPath)

mk :: String -> [FileSystem] -> [FileSystem] -> Maybe [FileSystem]
mk path toBeAdded ((MkDirectory name contents) : fs) =
    case getNextDirectory path of
        Just ("", "") ->
            Just (MkDirectory name (nub $ contents ++ toBeAdded) : fs)
        Just (rest, curr) ->
            case updateContent curr rest toBeAdded contents of
                (Just updatedContents) -> Just (MkDirectory name updatedContents : fs)
                Nothing -> Nothing
        Nothing -> Nothing
mk _ _ fs = Just fs

mkFile :: String -> [FileSystem] -> Maybe [FileSystem]
mkFile input fs = case wordParser input of
    Just (rest, curr) ->
        if isPath curr
            then mk curr (parseFiles rest) fs
            else mk "" (parseFiles (curr ++ " " ++ rest)) fs
    Nothing -> Nothing

    where
        parseFiles :: String -> [FileSystem]
        parseFiles input = case wordParser input of
            Just (rest, name) ->
                case eofParser rest of
                    Just (rest', content) -> MkFile name content : parseFiles rest'
                    Nothing -> []

mkDirectory :: String -> [FileSystem] -> Maybe [FileSystem]
mkDirectory input fs = case wordParser input of
    Just (rest, curr) ->
        if isPath curr
            then mk curr (parseFolders rest) fs
            else mk "" (MkDirectory curr [] : parseFolders rest) fs
    Nothing -> Nothing

    where
        parseFolders :: String -> [FileSystem]
        parseFolders rest = map (`MkDirectory` []) (words rest)

updateContent :: String -> String -> [FileSystem] -> [FileSystem] -> Maybe [FileSystem]
updateContent targetPath restPath toBeAdded (entry@(MkDirectory name contents) : fs)
    | name == targetPath =
        case mk restPath toBeAdded (entry : fs) of
            (Just updatedContents) -> Just updatedContents
            Nothing -> Nothing
    | otherwise =
        case updateContent targetPath restPath toBeAdded contents of
            (Just remaining) -> Just (entry : remaining)
            Nothing -> Nothing
updateContent _ _ _ [] = Nothing