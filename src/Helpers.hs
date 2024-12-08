module Helpers where

import FileSystem ( FileSystem(..) )

isPath :: String -> Bool
isPath = foldr (\ x -> (||) (x == '/')) False

-- maybeToList :: Maybe FileSystem -> Maybe [FileSystem]
-- maybeToList = fmap return

-- listToMaybe :: Maybe [FileSystem] -> Maybe FileSystem
-- listToMaybe = fmap (head .) . fmap (take 1)