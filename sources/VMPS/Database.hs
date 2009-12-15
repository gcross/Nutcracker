-- @+leo-ver=4-thin
-- @+node:gcross.20091211162553.1666:@thin Database.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091211162553.1667:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091211162553.1667:<< Language extensions >>
-- @nl

module VMPS.Database where

-- @<< Import needed modules >>
-- @+node:gcross.20091211162553.1668:<< Import needed modules >>
import Control.Applicative.Infix
import Control.DeepSeq
import Control.Monad
import Control.Monad.Error
import Control.Monad.Trans

import Data.ByteString (ByteString,unpack)
import Data.ByteString.Internal
import Data.Complex
import Data.ConfigFile
import Data.UUID
import Data.Word

import Database.Enumerator
import Database.PostgreSQL.Enumerator
import Database.PostgreSQL.PGFunctions

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import System.Exit
import System.IO.Unsafe
import System.Random

import VMPS.Miscellaneous
import VMPS.States
import VMPS.Tensors
-- @-node:gcross.20091211162553.1668:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091211162553.1669:Values
-- @+node:gcross.20091211162553.1670:digits
zero :: Word8
zero = 48
one = zero+1
two = one+1
three = two+1
four = three+1
five = four+1
six = five+1
seven = six+1
eight = seven+1
nine = eight+1
slash = 92
-- @-node:gcross.20091211162553.1670:digits
-- @-node:gcross.20091211162553.1669:Values
-- @+node:gcross.20091211162553.1671:Enumerators
-- @+node:gcross.20091211162553.1672:getX
get1 :: (Monad m) => a -> IterAct m (Maybe a)
get1 x _ = return $ Left $ Just $! x

get2 :: (Monad m) => a -> b -> IterAct m (Maybe (a,b))
get2 x y _ = return $ Left $ Just $! (x,y)
-- @-node:gcross.20091211162553.1672:getX
-- @+node:gcross.20091211162553.1673:fetchX
fetch1 :: (Monad m) => a -> IterAct m [a]
fetch1 a accum = result' (a:accum) --'

fetch2 :: (Monad m) => a -> b -> IterAct m [(a, b)]
fetch2 a b accum = result' ((a, b):accum) --'

fetch3 :: (Monad m) => a -> b -> c -> IterAct m [(a, b, c)]
fetch3 a b c accum = result' ((a, b, c):accum) --'

fetch4 :: (Monad m) => a -> b -> c -> d -> IterAct m [(a, b, c, d)]
fetch4 a b c d accum = result' ((a, b, c, d):accum) --'
-- @-node:gcross.20091211162553.1673:fetchX
-- @-node:gcross.20091211162553.1671:Enumerators
-- @+node:gcross.20091211162553.1674:Functions
-- @+node:gcross.20091211162553.1675:makeConnection
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
-- @-node:gcross.20091211162553.1675:makeConnection
-- @+node:gcross.20091211162553.1676:toEncodedString
toEncodedString :: (Pinnable a, StateSiteTensorClass a) => a -> String
toEncodedString tensor = unsafePerformIO $
    withStateSiteTensorAsByteString tensor $ \bytestring ->
        let string = map w2c . encode . unpack $ bytestring
        in string `deepseq` return string
-- @-node:gcross.20091211162553.1676:toEncodedString
-- @+node:gcross.20091211162553.1677:fromEncodedString
fromEncodedString :: (Creatable a (Int,Int,Int)) => Int -> Int -> Int -> String -> a
fromEncodedString physical_dimension left_bandwidth_dimension right_bandwidth_dimension string =
    snd . unsafePerformIO . withNewPinnedTensor (physical_dimension,left_bandwidth_dimension,right_bandwidth_dimension) $ \ptr ->
        let decoded_string = decode . map c2w $ string
            decoded_string_length = length decoded_string
            expected_length =
                physical_dimension *
                left_bandwidth_dimension *
                right_bandwidth_dimension *
                sizeOf (undefined :: Complex Double)
        in if decoded_string_length == expected_length
            then go (castPtr ptr) decoded_string
            else error $
                    "The string has the wrong length! ("
                    ++ show expected_length ++
                    " /= "
                    ++ show decoded_string_length ++
                    ")"
  where
    go _ [] = return ()
    go ptr (word:rest) = poke ptr word >> go (ptr `plusPtr` sizeOf (undefined :: Word8)) rest
-- @-node:gcross.20091211162553.1677:fromEncodedString
-- @+node:gcross.20091211162553.1678:withStateSiteTensorAsByteString
withStateSiteTensorAsByteString :: (Pinnable a, StateSiteTensorClass a) => a -> (ByteString -> IO b) -> IO b
withStateSiteTensorAsByteString tensor thunk =
    let size_in_elements = (physicalDimensionOfState <^(*)^> leftBandwidthOfState <^(*)^> rightBandwidthOfState) tensor
        size_in_bytes = size_in_elements * sizeOf (undefined :: Complex Double)
    in withPinnedTensor tensor $
        return . castPtr
        >=>
        newForeignPtr_
        >=>
        (\foreign_ptr -> return $ fromForeignPtr foreign_ptr 0 size_in_bytes)
        >=>
        thunk
-- @-node:gcross.20091211162553.1678:withStateSiteTensorAsByteString
-- @+node:gcross.20091211162553.1679:decode
decode :: [Word8] -> [Word8]
decode [] = []
decode (92:92:rest) = 92:decode rest
decode (92:a:b:c:rest) = ((a-zero)*64+(b-zero)*8+(c-zero)):decode rest
decode (x:rest) = x:decode rest
-- @-node:gcross.20091211162553.1679:decode
-- @+node:gcross.20091211162553.1680:encode
encode :: [Word8] -> [Word8]
encode [] = []
encode (0:rest) = slash:zero:zero:zero:encode rest
encode (39:rest) = slash:zero:four:seven:encode rest
encode (92:rest) = slash:one:three:four:encode rest
encode (other:rest)
    | (0 <= other && other <= 31) || (127 <= other && other <= 255)
      = let (a,remainder) = other `divMod` 64
            (b,c) = remainder `divMod` 8
        in slash:(a+zero):(b+zero):(c+zero):encode rest
    | otherwise
      = other:encode rest
-- @-node:gcross.20091211162553.1680:encode
-- @+node:gcross.20091211162553.1681:deslash
deslash :: [Word8] -> [Word8]
deslash [] = []
deslash (92:92:rest) = 92:deslash rest
deslash (other:rest) = other:deslash rest
-- @-node:gcross.20091211162553.1681:deslash
-- @+node:gcross.20091211162553.1682:stateIteratee
stateIteratee :: (MonadIO m) =>
    Int -> Int -> Int -> -- dimensions
    String -> -- data
    Int -> -- site number
    IterAct m (Either [RightAbsorptionNormalizedStateSiteTensor] CanonicalStateRepresentation)
stateIteratee
    physical_dimension left_bandwidth_dimension right_bandwidth_dimension
    site_data
    0
    (Left right_sites)
    = return
        .
        Left
        .
        Right
        .
        flip (CanonicalStateRepresentation (length right_sites + 1)) right_sites
        $!
        fromEncodedString physical_dimension left_bandwidth_dimension right_bandwidth_dimension site_data
stateIteratee
    physical_dimension left_bandwidth_dimension right_bandwidth_dimension
    site_data
    _
    (Left right_sites)
    = return
        .
        Right
        .
        Left
        .
        (:right_sites)
        $!
        fromEncodedString physical_dimension left_bandwidth_dimension right_bandwidth_dimension site_data
-- @-node:gcross.20091211162553.1682:stateIteratee
-- @+node:gcross.20091211162553.1683:generateRandomUUIDAsString
generateRandomUUIDAsString :: (MonadIO m) => m String
generateRandomUUIDAsString = liftIO (fmap show (randomIO :: IO UUID))

-- @-node:gcross.20091211162553.1683:generateRandomUUIDAsString
-- @+node:gcross.20091211162553.1684:fetchState
fetchState state_id =
    doQuery
        (sql $ "select physical_dimension, left_bandwidth_dimension, right_bandwidth_dimension, data, site_number from state_site_tensors where state_id = '" ++ state_id ++ "' order by site_number desc;")
        stateIteratee
        (Left [])
    >>= \result ->
        case result of
            Left [] -> return Nothing
            Right state -> return (Just state)
-- @-node:gcross.20091211162553.1684:fetchState
-- @+node:gcross.20091211162553.1685:insertRows
insertRows name statement type_ids rows =
    withPreparedStatement
        (prepareCommand name (sql statement) type_ids)
        (\prepared_statement ->
            (forM rows $
                \row -> withBoundStatement prepared_statement row $
                    \bound_statement -> execDML bound_statement
            )
            >>=
            (return . sum)
        )
    >>=
    \number_of_rows_inserted ->
        unless (number_of_rows_inserted == length rows) . error $
            "Inserted "
            ++ show number_of_rows_inserted ++
            " rows, but had been given "
            ++ show (length rows) ++
            " rows."
-- @-node:gcross.20091211162553.1685:insertRows
-- @+node:gcross.20091211162553.1686:storeState
storeState (CanonicalStateRepresentation number_of_sites first_tensor rest_tensors) =
    generateRandomUUIDAsString
    >>=
    (\state_id ->
        let toRow :: (Pinnable a, StateSiteTensorClass a, DBBind String s stmt bo, DBBind Int s stmt bo) =>
                     Int -> a -> [BindA s stmt bo]
            toRow site_number site_tensor =
                [bindP $ state_id
                ,bindP $ site_number
                ,bindP $ physicalDimensionOfState site_tensor
                ,bindP $ leftBandwidthOfState site_tensor
                ,bindP $ rightBandwidthOfState site_tensor
                ,bindP $ toEncodedString site_tensor
                ]
            rows = toRow 0 first_tensor:zipWith toRow [1..] rest_tensors
        in insertRows
                "insert_tensor"
                "insert into state_site_tensors (state_id, site_number, physical_dimension, left_bandwidth_dimension, right_bandwidth_dimension, data) values ((?::uuid),?,?,?,?,(?::bytea))"
                [pgTypeOid (undefined :: String)
                ,pgTypeOid (undefined :: Int)
                ,pgTypeOid (undefined :: Int)
                ,pgTypeOid (undefined :: Int)
                ,pgTypeOid (undefined :: Int)
                ,pgTypeOid (undefined :: String)
                ]
                rows
            >>
            return state_id
    )
-- @-node:gcross.20091211162553.1686:storeState
-- @+node:gcross.20091211162553.1687:storeSolution
storeSolution levels = do
    solution_id <- generateRandomUUIDAsString
    rows <- forM (zip [0..] levels) $
        \(level_number,(energy,state)) ->
            storeState state
            >>=
            \state_id -> return
                [bindP (solution_id :: String)
                ,bindP (level_number :: Int)
                ,bindP (state_id :: String)
                ,bindP (energy :: Double)
                ]
    insertRows
        "insert_solution_level"
        "insert into solutions (solution_id, level_number, state_id, energy) values ((?::uuid),?,(?::uuid),?)"
        [pgTypeOid (undefined :: String)
        ,pgTypeOid (undefined :: Int)
        ,pgTypeOid (undefined :: String)
        ,pgTypeOid (undefined :: Double)
        ]
        rows
    return solution_id
-- @-node:gcross.20091211162553.1687:storeSolution
-- @-node:gcross.20091211162553.1674:Functions
-- @-others
-- @-node:gcross.20091211162553.1666:@thin Database.hs
-- @-leo
