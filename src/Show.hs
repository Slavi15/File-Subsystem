module Show where

import Data.List (find)

import Core.FileSystem ( FileSystem(..) )

import Output (printFile)
import Utility (getName)

showFile :: String -> [FileSystem] -> String 
showFile name ((MkDirectory _ contents) : _) = 
    case find (\entry -> getName entry == name) contents of
        (Just result) -> printFile result
        Nothing -> "File does not exist!\n"