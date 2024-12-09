module Utility where

import Core.FileSystem ( FileSystem(..) )

isPath :: String -> Bool
isPath = foldr (\ x -> (||) (x == '/')) False

isFile :: FileSystem -> Bool
isFile (MkFile _ _) = True
isFile _ = False

isDirectory :: FileSystem -> Bool
isDirectory (MkDirectory _ _) = True
isDirectory _ = False

getName :: Maybe FileSystem -> String
getName (Just (MkDirectory name _)) = name
getName (Just (MkFile name _)) = name