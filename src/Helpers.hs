module Helpers where

import FileSystem ( FileSystem(..) )

isPath :: String -> Bool
isPath = foldr (\ x -> (||) (x == '/')) False