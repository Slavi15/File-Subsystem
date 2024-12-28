module Interact where

readFileContent :: IO String
readFileContent = do
    line <- getLine
    if line == "."
        then pure ""
        else do
            rest <- readFileContent
            pure (line ++ "\n" ++ rest)