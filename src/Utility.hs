module Utility where

import Data.FileSystem ( FileSystem(..) )

isPath :: String -> Bool
isPath = foldr (\ x -> (||) (x == '/')) False

isFile :: FileSystem -> Bool
isFile (MkFile _ _) = True
isFile _ = False

isDirectory :: FileSystem -> Bool
isDirectory (MkDirectory _ _) = True
isDirectory _ = False

getName :: FileSystem -> String
getName (MkFile name _) = name
getName (MkDirectory name _) = name