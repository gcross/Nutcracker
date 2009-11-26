-- @+leo-ver=4-thin
-- @+node:gcross.20091124193852.1643:@thin PostgreSQL.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091124193852.1644:<< Language extensions >>
{-# LANGUAGE FlexibleContexts #-}
-- @-node:gcross.20091124193852.1644:<< Language extensions >>
-- @nl

module VMPS.HDBC.PostgreSQL where

-- @<< Import needed modules >>
-- @+node:gcross.20091124193852.1645:<< Import needed modules >>
import Prelude hiding (concat,map,(++))

import Control.Monad

import Data.ByteString (ByteString,pack,unpack)
import Data.ByteString.Fusion
import Data.List.Stream
import Data.UUID
import Data.Word

import Database.HDBC

import System.Random

import VMPS.HDBC as HDBC
import VMPS.Miscellaneous
import VMPS.States
import VMPS.Tensors
-- @-node:gcross.20091124193852.1645:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091124193852.1663:Values
-- @+node:gcross.20091124193852.1664:characters
zero = 48
backslash = 92
-- @nonl
-- @-node:gcross.20091124193852.1664:characters
-- @-node:gcross.20091124193852.1663:Values
-- @+node:gcross.20091124193852.1646:Functions
-- @+node:gcross.20091124193852.1647:withSQLRowForStateSiteTensor
withSQLRowForStateSiteTensor :: (StateSiteTensorClass a, Pinnable a) => SqlValue -> Int -> a -> ([SqlValue] -> IO b) -> IO b
withSQLRowForStateSiteTensor = HDBC.withSQLRowForStateSiteTensorWithEscape escape
-- @-node:gcross.20091124193852.1647:withSQLRowForStateSiteTensor
-- @+node:gcross.20091124193852.1648:stateSiteTensorFromSQLRow
stateSiteTensorFromSQLRow :: (Creatable a (Int,Int,Int)) => [SqlValue] -> a
stateSiteTensorFromSQLRow = HDBC.stateSiteTensorFromSQLRowWithUnescape unescape
-- @-node:gcross.20091124193852.1648:stateSiteTensorFromSQLRow
-- @+node:gcross.20091124193852.1649:saveState
saveState :: IConnection conn => conn -> CanonicalStateRepresentation -> IO UUID
saveState = HDBC.saveStateWithEscape escape
-- @-node:gcross.20091124193852.1649:saveState
-- @+node:gcross.20091124193852.1650:loadState
loadState :: IConnection conn => conn -> UUID -> IO CanonicalStateRepresentation
loadState = loadStateWithUnescape unescape
-- @-node:gcross.20091124193852.1650:loadState
-- @+node:gcross.20091124193852.1659:escape/unescape
escape = pack . (concat . map ((backslash:).toOctet)) . unpack

unescape = pack . go . unpack
  where
    go :: [Word8] -> [Word8]
    go (92:92:rest) = backslash:go rest
    go (92:c1:c2:c3:rest) = fromOctet c1 c2 c3:go rest
    go (92:bad_sequence) = error $ "invalid escape sequence " ++ show bad_sequence
    go (c:rest) = c:go rest
    go [] = []

-- @-node:gcross.20091124193852.1659:escape/unescape
-- @+node:gcross.20091124193852.1660:toOctal/fromOctal
toOctet :: Word8 -> [Word8]
toOctet c =
    let (digit1,remainder1) = c `divMod` 64
        (digit2,digit3) = remainder1 `divMod` 8
    in [zero+digit1,zero+digit2,zero+digit3]

fromOctet :: Word8 -> Word8 -> Word8 -> Word8
fromOctet c1 c2 c3 = (c1-zero)*64+(c2-zero)*8+(c3-zero)
-- @-node:gcross.20091124193852.1660:toOctal/fromOctal
-- @-node:gcross.20091124193852.1646:Functions
-- @-others
-- @-node:gcross.20091124193852.1643:@thin PostgreSQL.hs
-- @-leo
