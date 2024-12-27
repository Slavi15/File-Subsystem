module Core.Command where

data MKCommands = MkDir | Touch
    deriving (Eq, Show)

data Command = PWDCommand 
    | CDCommand 
    | LSCommand 
    | DIRCommand MKCommands
    | CATCommand 
    | RMCommand
    | SHOWCommand
    | QUITCommand
    deriving (Eq, Show)