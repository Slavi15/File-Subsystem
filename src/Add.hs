module Add where

import Core.FileSystem ( FileSystem(..) )

import Parser (wordParser, getNextDirectory, eofParser)
import Utility (isPath)

mk :: String -> [FileSystem] -> [FileSystem] -> Maybe [FileSystem]
mk "" toBeAdded ((MkDirectory name contents) : fs) =
    Just [MkDirectory name (contents ++ toBeAdded)]
mk path toBeAdded ((MkDirectory name contents) : fs) =
    case getNextDirectory path of
        Just (rest, curr) ->
            case updateContent curr rest toBeAdded contents of
                (Just updatedContents) -> Just [MkDirectory name updatedContents]
                Nothing -> Nothing
        Nothing -> Just [MkDirectory name contents]
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
                    Nothing -> [MkFile name ""]

mkDirectory :: String -> [FileSystem] -> Maybe [FileSystem]
mkDirectory input fs = case wordParser input of
    Just (rest, curr) ->
        if isPath curr
            then mk curr parsedFolders fs
            else mk "" (MkDirectory curr [] : parsedFolders) fs
        where
            parsedFolders = map (`MkDirectory` []) (words rest)
    Nothing -> Nothing

updateContent :: String -> String -> [FileSystem] -> [FileSystem] -> Maybe [FileSystem]
updateContent targetPath restPath toBeAdded (entry@(MkDirectory name contents) : fs)
    | name == targetPath =
        case mk restPath toBeAdded (entry : fs) of
            (Just updatedDirectory) -> Just (updatedDirectory ++ fs)
            Nothing -> Nothing
    | otherwise =
        case updateContent targetPath restPath toBeAdded fs of
            (Just remaining) -> Just (entry : remaining)
            Nothing -> Nothing
updateContent _ _ _ [] = Nothing