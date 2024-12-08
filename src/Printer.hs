module Printer where

import Data.FileSystem ( FileSystem(..) )

printFile :: FileSystem -> String
printFile (MkFile name content) = "File: " ++ name ++ "\nContent: \n" ++ content ++ "\n"
printFile _ = ""

printDirectory :: [FileSystem] -> String
printDirectory ((MkDirectory "/" _) : fs) = "/" ++ printDirectory fs
printDirectory ((MkDirectory name _) : fs) = name ++ "/" ++ printDirectory fs
printDirectory _ = ""