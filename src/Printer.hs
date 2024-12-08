module Printer where

import FileSystem ( FileSystem(..) )

printFile :: FileSystem -> String
printFile (MkFile name content) = "File: " ++ name ++ "\nContent: \n" ++ content ++ "\n"
printFile _ = ""

printDirectory :: [FileSystem] -> String
printDirectory ((MkFile "/" _) : fs) = "/" ++ printDirectory fs
printDirectory ((MkFile name _) : fs) = name ++ "/" ++ printDirectory fs
printDirectory _ = ""