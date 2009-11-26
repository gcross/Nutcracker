-- @+leo-ver=4-thin
-- @+node:gcross.20091124153705.2067:@thin HDBC.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091124153705.2068:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091124153705.2068:<< Language extensions >>
-- @nl

module VMPS.HDBC where

-- @<< Import needed modules >>
-- @+node:gcross.20091124153705.2069:<< Import needed modules >>
import Control.Monad

import Data.ByteString (ByteString)
import Data.UUID

import Database.HDBC

import System.Random

import VMPS.Miscellaneous
import VMPS.States
import VMPS.Tensors
-- @-node:gcross.20091124153705.2069:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091124153705.2073:Functions
-- @+node:gcross.20091124153705.2401:withSQLRowForStateSiteTensorWithEscape
withSQLRowForStateSiteTensorWithEscape :: (StateSiteTensorClass a, Pinnable a) => (ByteString -> ByteString) -> SqlValue -> Int -> a -> ([SqlValue] -> IO b) -> IO b
withSQLRowForStateSiteTensorWithEscape escape uuid site_number state_site_tensor thunk =
    withPinnedTensorAsByteString state_site_tensor $ \state_data ->
        thunk
            [uuid
            ,iToSql site_number
            ,iToSql . physicalDimensionOfState $ state_site_tensor
            ,iToSql . leftBandwidthOfState $ state_site_tensor
            ,iToSql . rightBandwidthOfState $ state_site_tensor
            ,SqlByteString . escape $ state_data
            ]
-- @-node:gcross.20091124153705.2401:withSQLRowForStateSiteTensorWithEscape
-- @+node:gcross.20091124193852.1654:withSQLRowForStateSiteTensor
withSQLRowForStateSiteTensor :: (StateSiteTensorClass a, Pinnable a) => SqlValue -> Int -> a -> ([SqlValue] -> IO b) -> IO b
withSQLRowForStateSiteTensor = withSQLRowForStateSiteTensorWithEscape id
-- @-node:gcross.20091124193852.1654:withSQLRowForStateSiteTensor
-- @+node:gcross.20091124193852.1633:stateSiteTensorFromSQLRowWithUnescape
stateSiteTensorFromSQLRowWithUnescape :: (Creatable a (Int,Int,Int)) => (ByteString -> ByteString) -> [SqlValue] -> a
stateSiteTensorFromSQLRowWithUnescape unescape [physical_dimension,left_bandwidth_dimension,right_bandwidth_dimension,data_] =
    tensorFromByteString
        ((fromSql physical_dimension :: Int)
        ,(fromSql left_bandwidth_dimension :: Int)
        ,(fromSql right_bandwidth_dimension :: Int)
        )
        (unescape . fromSql $ data_)
stateSiteTensorFromSQLRowWithUnescape _ other = error $ "wrong number of columns received in this row;  expected 4 but got " ++ show (length other)
-- @-node:gcross.20091124193852.1633:stateSiteTensorFromSQLRowWithUnescape
-- @+node:gcross.20091124193852.1652:stateSiteTensorFromSQLRow
stateSiteTensorFromSQLRow :: (Creatable a (Int,Int,Int)) => [SqlValue] -> a
stateSiteTensorFromSQLRow = stateSiteTensorFromSQLRowWithUnescape id
-- @-node:gcross.20091124193852.1652:stateSiteTensorFromSQLRow
-- @+node:gcross.20091124193852.1656:saveStateWithEscape
saveStateWithEscape :: IConnection conn => (ByteString -> ByteString) -> conn -> CanonicalStateRepresentation -> IO UUID
saveStateWithEscape escape connection state = do
    state_uuid <- randomIO
    let state_uuid_as_sql_value = SqlString . show $ state_uuid
        go :: Int -> [[SqlValue]] -> [RightAbsorptionNormalizedStateSiteTensor] -> [SqlValue] -> IO ()
        go _ accumulated_rows [] current_row =
            prepare connection "insert into state_site_tensors (state_id,site_number,physical_dimension,left_bandwidth_dimension,right_bandwidth_dimension,data) values (?,?,?,?,?,?)"
            >>=
            flip executeMany (current_row:accumulated_rows)
        go next_row_number accumulated_rows (next_tensor:remaining_tensors) current_row =
            withSQLRowForStateSiteTensorWithEscape escape state_uuid_as_sql_value next_row_number next_tensor $
                go (next_row_number+1) (current_row:accumulated_rows) remaining_tensors
    withSQLRowForStateSiteTensor state_uuid_as_sql_value 1 (canonicalStateFirstSiteTensor state) $
        go 1 [] (canonicalStateRestSiteTensors state)
    return state_uuid
-- @-node:gcross.20091124193852.1656:saveStateWithEscape
-- @+node:gcross.20091124153705.2074:saveState
saveState :: IConnection conn => conn -> CanonicalStateRepresentation -> IO UUID
saveState = saveStateWithEscape id
-- @-node:gcross.20091124153705.2074:saveState
-- @+node:gcross.20091124193852.1631:loadStateWithUnescape
loadStateWithUnescape :: IConnection conn => (ByteString -> ByteString) -> conn -> UUID -> IO CanonicalStateRepresentation
loadStateWithUnescape unescape connection state_uuid = do
    quickQuery
        connection 
        (echo $ "select physical_dimension,left_bandwidth_dimension,right_bandwidth_dimension,data from state_site_tensors where state_id='" ++ (show state_uuid) ++ "' order by site_number desc")
        []
    >>=
    return . go 1 []
  where
    go _ _ [] = error "No tensors found for this state!"
    go number_of_sites right_normalized_tensors (unnormalized_tensor_row:[]) =
        CanonicalStateRepresentation
            number_of_sites
            (stateSiteTensorFromSQLRowWithUnescape unescape unnormalized_tensor_row)
            right_normalized_tensors
    go number_of_sites right_normalized_tensors (right_normalized_tensor_row:rest_rows) =
        let right_normalized_tensor = stateSiteTensorFromSQLRowWithUnescape unescape right_normalized_tensor_row
        in go (number_of_sites+1) (right_normalized_tensor:right_normalized_tensors) rest_rows
-- @-node:gcross.20091124193852.1631:loadStateWithUnescape
-- @+node:gcross.20091124193852.1658:loadState
loadState :: IConnection conn => conn -> UUID -> IO CanonicalStateRepresentation
loadState = loadStateWithUnescape id
-- @-node:gcross.20091124193852.1658:loadState
-- @-node:gcross.20091124153705.2073:Functions
-- @-others
-- @-node:gcross.20091124153705.2067:@thin HDBC.hs
-- @-leo
