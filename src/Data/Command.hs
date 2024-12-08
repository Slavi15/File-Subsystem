module Data.Command where

data MKCommands = MkDir | Touch
    deriving (Eq, Show)

data RMCommands = RM | RmDir
    deriving (Eq, Show)

data Command = PWDCommand 
    | CDCommand 
    | LSCommand 
    | DIRCommand MKCommands
    | CATCommand 
    | RMCommand RMCommands
    | QUITCommand
    deriving (Eq, Show)