module Core.Eval where

import Core.FileSystem ( FileSystem(..) )

data Eval = Continue (Maybe [FileSystem])
    | PWD
    | LS String
    | SHOW String
    | QUIT