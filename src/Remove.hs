module Remove where

import Core.FileSystem ( FileSystem(..) )

import Utility (getName, isFile)

rm :: (FileSystem -> Bool) -> [String] -> [FileSystem] -> Maybe [FileSystem]
rm predicate (toRemove : toBeRemoved) ((MkDirectory name contents) : fs) =
    rm predicate toBeRemoved (MkDirectory name updatedContents : fs)
    where
        updatedContents :: [FileSystem]
        updatedContents = filter (\entry -> getName entry /= toRemove || not (predicate entry)) contents
rm _ _ fs = Just fs

rmFiles :: String -> [FileSystem] -> Maybe [FileSystem]
rmFiles input = rm isFile (words input)