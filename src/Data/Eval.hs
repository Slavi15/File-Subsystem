module Data.Eval where

import Data.FileSystem ( FileSystem(..) )

data Eval = Continue (Maybe FileSystem)
    | PWD
    | Quit