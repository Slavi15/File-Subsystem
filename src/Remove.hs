module Remove where

import Core.FileSystem ( FileSystem(..) )
import Utility (getName)

rm :: [FileSystem] -> [FileSystem] -> Maybe [FileSystem]
rm (toRemove : toBeRemoved) ((MkDirectory name contents) : _) = 
    rm toBeRemoved [MkDirectory name (filter (\entry -> getName (Just entry) /= getName (Just toRemove)) contents)]
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