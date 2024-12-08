module Remove where

import Data.FileSystem ( FileSystem(..) )

import Utility (getName)

rm :: [FileSystem] -> Maybe FileSystem -> Maybe FileSystem
rm toBeRemoved (Just (MkDirectory name contents)) =
    Just $ MkDirectory name (filter (\entry -> getName entry `notElem` removeNames) contents)
    where
        removeNames :: [String]
        removeNames = map getName toBeRemoved
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