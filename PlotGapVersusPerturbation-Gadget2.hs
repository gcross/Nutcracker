-- @+leo-ver=4-thin
-- @+node:gcross.20091023155936.1481:@thin PlotGapVersusPerturbation-Gadget2.hs
-- @@language Haskell

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Data.Char
import Graphics.Gnuplot.Simple
import System
import System.IO
import System.Console.GetOpt
import Text.Printf

import Database

-- @<< Options handling >>
-- @+node:gcross.20091023155936.1482:<< Options handling >>
options :: [OptDescr String]
options =
 [ Option ['m']     ["minimum-bound"] -- '
     (ReqArg (\ n -> " and perturbation_strength > " ++ n)
             "#")
     "minimum bound on the perturbation strength"
 , Option ['M']     ["maximum-bound"] -- '
     (ReqArg (\ n -> " and perturbation_strength < " ++ n)
             "#")
     "maximum bound on the perturbation strength"
 ]

defaultOptions = "true"

parseOptions :: [String] -> (String,[String])
parseOptions args = 
    case getOpt Permute options args of
        (_,[],errs) -> error (concat errs ++ usageInfo header options)
        (o,number_of_sites_list,[]  ) -> (concat o,number_of_sites_list)
        (_,_,errs) -> error (concat errs ++ usageInfo header options)
    where header = "\n\nUsage: PlotGapVersusPerturbation [-m #] [-M #] <number of sites> <number of sites> ..."
-- @-node:gcross.20091023155936.1482:<< Options handling >>
-- @nl

main =
    makeConnection "reader"
    >>=
    flip withSession (do
        (filter_string,number_of_sites_list) <- liftIO getArgs >>= return . parseOptions
        forM number_of_sites_list $ \number_of_sites ->
            let sql_statement = "select perturbation_strength, energy_gap from gadget_model_simulations where model_id = 2 and number_of_sites = " ++ number_of_sites ++ filter_string ++ " order by perturbation_strength desc;"
            in do
                liftIO . hPutStrLn stderr $ "> " ++ sql_statement
                (data_points :: [(Float,Float)]) <- query (sql $ sql_statement) fetch2 [] "Error fetching completed graphs from the database:\n"
                return (number_of_sites,data_points)
    )
    >>=
    return . concatMap (\(color,(number_of_sites,data_points)) ->
        [ (PlotStyle Points (CustomStyle [LineTitle (number_of_sites), LineType color, PointType 2]),data_points)
        ]
    ) . zip [1..]
    >>=
    plotPathsStyle
        [Custom "logscale" ["xy"]
        ,Key (Just ["left","top"])
        ,Title "Energy Gap"
        ,XLabel "Perturbation Strength"
        ,YLabel "Energy Gap (Delta = 1)"
        ]
-- @-node:gcross.20091023155936.1481:@thin PlotGapVersusPerturbation-Gadget2.hs
-- @-leo
