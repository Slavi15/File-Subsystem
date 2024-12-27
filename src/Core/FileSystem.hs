module Core.FileSystem where

data FileSystem =
    MkFile String String 
    | MkDirectory String [FileSystem]
    deriving (Eq, Show)