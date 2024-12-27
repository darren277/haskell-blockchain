module AppTypes where

import Data.IORef
import Data.Pool
import qualified Database.PostgreSQL.Simple as PG
import Blockchain (Block)

data AppState = AppState 
    { blockchain :: IORef [Block]
    , dbPool :: Pool PG.Connection 
    }