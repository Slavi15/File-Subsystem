module Utility where

import Core.FileSystem ( FileSystem(..) )

isPath :: String -> Bool
isPath = foldr (\ x -> (||) (x == '/')) False

isAbsolutePath :: String -> Bool
isAbsolutePath input = isPath input && head input == '/'

isFile :: FileSystem -> Bool
isFile (MkFile _ _) = True
isFile _ = False

isDirectory :: FileSystem -> Bool
isDirectory (MkDirectory _ _) = True
isDirectory _ = False

getName :: FileSystem -> String
getName (MkDirectory name _) = name
getName (MkFile name _) = name