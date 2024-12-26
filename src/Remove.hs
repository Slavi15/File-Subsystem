module Remove where

import Core.FileSystem ( FileSystem(..) )

import Utility (getName)

rm :: [FileSystem] -> [FileSystem] -> Maybe [FileSystem]
rm (toRemove : toBeRemoved) ((MkDirectory name contents) : fs) =
    case filter (\entry -> getName entry /= getName toRemove) contents of
        updatedDirectory -> rm toBeRemoved (MkDirectory name updatedDirectory : fs)
rm _ fs = Just fs

rmFile :: String -> [FileSystem] -> Maybe [FileSystem]
rmFile input = rm files
    where
        files :: [FileSystem]
        files = map (`MkFile` "") (words input)

rmDirectory :: String -> [FileSystem] -> Maybe [FileSystem]
rmDirectory input = rm dirs
    where
        dirs :: [FileSystem]
        dirs = map (`MkDirectory` []) (words input)