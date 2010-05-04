-- @+leo-ver=4-thin
-- @+node:gcross.20091130053756.1966:@thin Setup.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091130053756.1967:<< Language extensions >>
{-# LANGUAGE PackageImports #-}
-- @-node:gcross.20091130053756.1967:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091130053756.1968:<< Import needed modules >>
import Control.Applicative

import System.FilePath

import Blueprint.Configuration
import Blueprint.Tools.GCC
import Blueprint.Tools.GHC.Main
import Blueprint.Tools.GFortran
-- @nonl
-- @-node:gcross.20091130053756.1968:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091130053756.1971:Types
-- @+node:gcross.20091130053756.1972:AdditionalConfiguration
data AdditionalConfiguration = AdditionalConfiguration
    {   gccConfiguration :: GCCConfiguration
    ,   gfortranConfiguration :: GFortranConfiguration
    }
-- @-node:gcross.20091130053756.1972:AdditionalConfiguration
-- @-node:gcross.20091130053756.1971:Types
-- @+node:gcross.20091214215701.2065:Functions
-- @+node:gcross.20091214215701.2074:configureAdditional
configureAdditional =
    AdditionalConfiguration
        <$> (configureUsingSection "GCC")
        <*> (configureUsingSection "GCC")
-- @-node:gcross.20091214215701.2074:configureAdditional
-- @+node:gcross.20091214215701.2076:compileAdditional
compileAdditional _ configuration build_root digest_cache_subdirectory object_subdirectory interface_subdirectory =
    gfortranCompileAll
        (gfortranConfiguration configuration)
        digest_cache_subdirectory
        gfortran_flags
        object_subdirectory
        (interface_subdirectory </> "fortran")
    .
    gccCompileAll
        (gccConfiguration configuration)
        digest_cache_subdirectory
        gcc_flags
        object_subdirectory
-- @-node:gcross.20091214215701.2076:compileAdditional
-- @-node:gcross.20091214215701.2065:Functions
-- @+node:gcross.20091130053756.1973:Values
-- @+node:gcross.20091212120817.2106:Additional Options
additional_options =
    [   gccOptions
    ,   gfortranOptions
    ]
-- @-node:gcross.20091212120817.2106:Additional Options
-- @+node:gcross.20091212120817.2108:Flags
ghc_flags = ["-O2","-fvia-C","-optc=-O3"]

flags = ["-O3","-ffast-math","-funroll-loops"]
gcc_flags = flags
gfortran_flags = "-cpp":"-fimplicit-none":flags
-- @-node:gcross.20091212120817.2108:Flags
-- @-node:gcross.20091130053756.1973:Values
-- @+node:gcross.20091130053756.1984:main
main =
    defaultMain
        configureAdditional
        compileAdditional
        additional_options
        ("","sources")
        (Just
           (("","tests/haskell")
           ,[("core","o"),("core-wrapper","o")]
           ,["HUnit == 1.*"
            ,"QuickCheck == 2.*"
            ,"spoon == 0.3.*"
            ,"test-framework == 0.2.*"
            ,"test-framework-hunit == 0.2.*"
            ,"test-framework-quickcheck2 == 0.2.*"
            ]
           )
        )
        ghc_flags
-- @-node:gcross.20091130053756.1984:main
-- @-others
-- @-node:gcross.20091130053756.1966:@thin Setup.hs
-- @-leo
