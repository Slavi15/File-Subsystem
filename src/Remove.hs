module Remove where

import Core.FileSystem ( FileSystem(..) )

import Utility (getName)
import Data.Maybe (listToMaybe)

rm :: [FileSystem] -> Maybe FileSystem -> Maybe FileSystem
rm toBeRemoved (Just (MkDirectory name contents)) =
    Just $ MkDirectory name (filter (\entry -> getName (Just entry) `notElem` removeNames) contents)
    where
        removeNames :: [String]
        removeNames = [getName (listToMaybe toBeRemoved)]
rm _ fs = fs

rmFile :: String -> Maybe FileSystem -> Maybe FileSystem
rmFile input = rm files
    where
        files :: [FileSystem]
        files = map (`MkFile` "") (words input)

rmDirectory :: String -> Maybe FileSystem -> Maybe FileSystem
rmDirectory input = rm dirs
    where
        dirs :: [FileSystem]
        dirs = map (`MkDirectory` []) (words input)