-- @+leo-ver=4-thin
-- @+node:gcross.20091023155936.1528:@thin bitslayer-gadget-sweep.hs
-- @@language Haskell

import Control.Monad
import Data.Function
import System.IO
import System.Process
import Text.Printf

system_parameters = [
        (number_of_sites,perturbation_coefficient)
        |   number_of_sites <- [10,100]
        ,   perturbation_coefficient <- [0.001 * 2**i | i <- [0..7]]
    ]

parametersToScript :: (Int,Float) -> String
parametersToScript (number_of_sites,perturbation_coefficient) =
    let job_name = printf "vmps-%i-%f" number_of_sites perturbation_coefficient
    in unlines
        ["#PBS -d /home/gcross/Projects/QC/VMPS"
        ,"#PBS -N " ++ job_name
        ,"#PBS -e logs/err/" ++ job_name
        ,"#PBS -o logs/out/" ++ job_name
        ,"#PBS -v LD_LIBRARY_PATH=/usr/local/pgsql/lib"
        ,""
        ,printf "python simulate-gadget2.py %i %f" number_of_sites perturbation_coefficient
        ]

main = forM_ system_parameters $ \parameters -> do
    (Just stdin,_,_,_) <- createProcess $ (shell "qsub") { std_in = CreatePipe }
    hPutStrLn stdin . parametersToScript $ parameters
    hFlush stdin
    return ()
-- @-node:gcross.20091023155936.1528:@thin bitslayer-gadget-sweep.hs
-- @-leo
