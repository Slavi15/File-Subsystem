module Show where

import Data.List (find)

import Core.FileSystem ( FileSystem(..) )

import Output (printFile)
import Utility (getName)

showFile :: String -> Maybe FileSystem -> String 
showFile name (Just (MkDirectory _ contents)) = 
    case find (\entry -> getName (Just entry) == name) contents of
        (Just result) -> printFile result
        Nothing -> "File does not exist!\n"