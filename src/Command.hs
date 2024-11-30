module Command where

data MKCommands = MKDIR | TOUCH
    deriving (Eq, Show)

data Command = PWDCommand 
    | CDCommand 
    | LSCommand 
    | DIRCommand MKCommands
    | CATCommand 
    | RMCommand 
    | QUITCommand
    deriving (Eq, Show)