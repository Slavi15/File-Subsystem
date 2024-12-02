module Command where

data MKCommands = MkDir | Touch
    deriving (Eq, Show)

data Command = PWDCommand 
    | CDCommand 
    | LSCommand 
    | DIRCommand MKCommands
    | CATCommand 
    | RMCommand 
    | QUITCommand
    deriving (Eq, Show)