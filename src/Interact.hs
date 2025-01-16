module Interact where

readFileContent :: IO String
readFileContent = 
    getLine >>= \line ->
    if line == "."
        then pure ""
        else readFileContent >>= \rest -> pure $ line ++ "\n" ++ rest