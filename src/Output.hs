module Output where

import Core.FileSystem ( FileSystem(..) )

printEntity :: FileSystem -> String
printEntity (MkFile name _) = "File: " ++ name ++ "\n"
printEntity (MkDirectory name _) = "Directory: " ++ name ++ "\n"

printFile :: FileSystem -> String
printFile (MkFile name content) = "File: " ++ name ++ "\nContent: \n" ++ content ++ "\n"
printFile _ = ""

printDirectory :: [FileSystem] -> String
printDirectory [] = ""
printDirectory ((MkDirectory "/" _) : fs) = "/" ++ printDirectory fs
printDirectory ((MkDirectory n _) : fs) = "/" ++ reverse n ++ printDirectory fs

printCDCommand :: FileSystem -> String
printCDCommand (MkDirectory _ contents) = concatMap printEntity contents