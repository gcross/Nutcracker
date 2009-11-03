-- @+leo-ver=4-thin
-- @+node:gcross.20091022161927.1464:@thin bitslayer-gadget-updater.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Trans
import Data.Function
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import System.IO
import System.Process
import Text.Printf

import Database

launchProcessForSolutionId :: String -> IO ()
launchProcessForSolutionId solution_id =
    let job_name = "vmps-update-" ++ solution_id
        job_script = unlines
            ["#PBS -d /home/gcross/Projects/QC/VMPS"
            ,"#PBS -N " ++ job_name
            ,"#PBS -e logs/err/" ++ job_name
            ,"#PBS -o logs/out/" ++ job_name
            ,"#PBS -v LD_LIBRARY_PATH=/usr/local/pgsql/lib"
            ,""
            ,"python compare-gadget-to-desired-states.py " ++ solution_id
            ]
    in do
        putStrLn solution_id
        (Just stdin,_,_,_) <- createProcess $ (shell "qsub") { std_in = CreatePipe }
        hPutStrLn stdin job_script
        hFlush stdin
        return ()

main =
    makeConnection "reader"
    >>=
    flip withSession (
        let sql_statement = "select solution_id from gadget_model_simulations where model_id=5 and infidelity is null;"
        in do
            liftIO . hPutStrLn stderr $ "> " ++ sql_statement
            (solution_ids :: [String]) <- query (sql $ sql_statement) fetch1 [] "Error fetching solution ids from the database:\n"
            return solution_ids
    )
    >>=
    mapM_ launchProcessForSolutionId
-- @-node:gcross.20091022161927.1464:@thin bitslayer-gadget-updater.hs
-- @-leo
