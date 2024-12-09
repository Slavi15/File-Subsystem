module Output where

import Core.FileSystem ( FileSystem(..) )

printEntity :: FileSystem -> String
printEntity (MkFile name _) = "File: " ++ name ++ "\n"
printEntity (MkDirectory name _) = "Directory: " ++ name ++ "\n"

printFile :: FileSystem -> String
printFile (MkFile name content) = "File: " ++ name ++ "\nContent: \n" ++ content ++ "\n"
printFile _ = ""

printDirectory :: String -> [FileSystem] -> String
printDirectory "/" _ = "/"
printDirectory target ((MkDirectory name contents) : fs) 
    | target == name = name ++ "/"
    | name == "/" = "/" ++ printDirectory target contents
    | otherwise = name ++ "/" ++ printDirectory target contents
printDirectory _ _ = ""

printCDCommand :: FileSystem -> String
printCDCommand (MkDirectory _ contents) = concatMap printEntity contents