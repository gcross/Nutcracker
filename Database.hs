-- @+leo-ver=4-thin
-- @+node:gcross.20091020092327.1443:@thin Database.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

module Database where

-- @<< Imports >>
-- @+node:gcross.20091020092327.1444:<< Imports >>
import Control.Arrow
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans
import Control.Exception
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions
import Data.Array
import Data.ConfigFile
import Data.List (zipWith4)
import Data.Maybe
import Data.Time.Clock
import Data.Typeable
import Data.UUID
import System.Exit
import System.IO
import System.Process
import System.Random
import Text.Printf



-- @-node:gcross.20091020092327.1444:<< Imports >>
-- @nl

-- @+others
-- @+node:gcross.20091020092327.1446:__ToFloat
intToFloat :: Int -> Float
intToFloat = fromInteger . toInteger

doubleToFloat :: Double -> Float
doubleToFloat = fromRational . toRational
-- @-node:gcross.20091020092327.1446:__ToFloat
-- @+node:gcross.20091020092327.1447:makeConnection
makeConnection heading = do
    either_conn <- runErrorT $ do
        cp <- join $ liftIO $ readfile emptyCP "connection.cfg"
        host <- get cp "data source" "host"
        database <- get cp "data source" "database"
        user <- get cp heading "user"
        password <- get cp heading "password"
        return $ connect
            [   CAhost host
            ,   CAdbname database
            ,   CAuser user
            ,   CApassword password
            ]
    case either_conn of
        Left err -> do
            print err
            exitFailure
        Right conn -> return conn
-- @-node:gcross.20091020092327.1447:makeConnection
-- @+node:gcross.20091020092327.1448:enumerators
-- @+node:gcross.20091020092327.1449:getX
get1 :: (Monad m) => a -> IterAct m (Maybe a)
get1 x _ = return $ Left $ Just $! x

get2 :: (Monad m) => a -> b -> IterAct m (Maybe (a,b))
get2 x y _ = return $ Left $ Just $! (x,y)
-- @-node:gcross.20091020092327.1449:getX
-- @+node:gcross.20091020092327.1450:fetchX
fetch1 :: (Monad m) => a -> IterAct m [a]
fetch1 a accum = result' (a:accum) --'

fetch2 :: (Monad m) => a -> b -> IterAct m [(a, b)]
fetch2 a b accum = result' ((a, b):accum) --'

fetch3 :: (Monad m) => a -> b -> c -> IterAct m [(a, b, c)]
fetch3 a b c accum = result' ((a, b, c):accum) --'

fetch4 :: (Monad m) => a -> b -> c -> d -> IterAct m [(a, b, c, d)]
fetch4 a b c d accum = result' ((a, b, c, d):accum) --'
-- @-node:gcross.20091020092327.1450:fetchX
-- @-node:gcross.20091020092327.1448:enumerators
-- @+node:gcross.20091020092327.1451:sql wrappers
-- @+node:gcross.20091020092327.1452:query
query stmt accum init message = 
    catchDB (
            doQuery stmt accum init
          ) (reportRethrowMsg $ message ++ "\n")
-- @-node:gcross.20091020092327.1452:query
-- @+node:gcross.20091020092327.1453:modify
modify stmt message = catchDB (
        execDML stmt
    ) (reportRethrowMsg $ message ++ "\n")
-- @nonl
-- @-node:gcross.20091020092327.1453:modify
-- @-node:gcross.20091020092327.1451:sql wrappers
-- @-others
-- @-node:gcross.20091020092327.1443:@thin Database.hs
-- @-leo
