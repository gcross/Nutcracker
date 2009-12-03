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
import Control.Applicative.Infix
import Control.Exception
import Control.Monad
import Control.Parallel

import Data.ConfigFile hiding (options)
import Data.Either.Unwrap
import Data.ErrorMessage
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Version

import Distribution.Package
import qualified Distribution.PackageDescription as Package

import System.Directory
import System.FilePath
import System.IO.Unsafe
import System.Process

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>),(</>))

import Blueprint.Configuration
import Blueprint.Main
import Blueprint.Miscellaneous
import Blueprint.Options
import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GCC
import Blueprint.Tools.GFortran
import Blueprint.Tools.GHC
import Blueprint.Tools.Installer
import Blueprint.Tools.Ld
-- @-node:gcross.20091130053756.1968:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091130053756.1969:Options
options =
    [   installerOptions
    ,   arOptions
    ,   ldOptions
    ,   ghcOptions
    ,   gccOptions
    ,   gfortranOptions
    ]
-- @-node:gcross.20091130053756.1969:Options
-- @+node:gcross.20091130053756.1970:Flags
ghc_flags = testing_ghc_flags ++ ["-package-name="++qualified_package_name]
testing_ghc_flags = ["-O2","-fvia-C","-optc=-O3"]

flags = ["-O3","-ffast-math","-funroll-loops"]
gcc_flags = flags
gfortran_flags = "-cpp":"-fimplicit-none":flags
-- @-node:gcross.20091130053756.1970:Flags
-- @+node:gcross.20091130053756.1971:Types
-- @+node:gcross.20091130053756.1972:Configuration
data Configuration = Configuration
    {   ghcConfiguration :: GHCConfiguration
    ,   gccConfiguration :: GCCConfiguration
    ,   gfortranConfiguration :: GFortranConfiguration
    ,   arConfiguration :: ArConfiguration
    ,   ldConfiguration :: LdConfiguration
    ,   installerConfiguration :: InstallerConfiguration
    ,   packageDependencies :: [String]
    ,   packageModules :: PackageModules
    }
-- @-node:gcross.20091130053756.1972:Configuration
-- @-node:gcross.20091130053756.1971:Types
-- @+node:gcross.20091130053756.1973:Values
-- @+node:gcross.20091130053756.1974:source resources
source_resources = sourceResourcesIn "sources"
-- @-node:gcross.20091130053756.1974:source resources
-- @+node:gcross.20091130053756.1975:package description
package_description = readPackageDescription "VMPS.cabal"
-- @-node:gcross.20091130053756.1975:package description
-- @+node:gcross.20091130053756.1976:configuration file path
configurationFilePath = "VMPS.cfg"
-- @-node:gcross.20091130053756.1976:configuration file path
-- @+node:gcross.20091130053756.1977:qualified package name
qualified_package_name =
    let PackageIdentifier (PackageName name) version = Package.package package_description
    in name ++ "-" ++ showVersion version
-- @-node:gcross.20091130053756.1977:qualified package name
-- @-node:gcross.20091130053756.1973:Values
-- @+node:gcross.20091130053756.1978:Targets
targets =
    [target "configure" configure
    ,target "reconfigure" $ makeReconfigureTarget configurationFilePath targets
    ,target "build" build
    ,target "rebuild" $ makeRebuildTarget targets
    ,target "install" install
    ,target "clean" $
        makeCleanTarget
            ["objects"
            ,"digest-cache"
            ,"haskell-interfaces"
            ,"libraries"
            ,"tests/haskell/objects"
            ,"tests/haskell/digest-cache"
            ,"tests/haskell/haskell-interfaces"
            ]
    ,target "distclean" $
        makeDistCleanTarget
            [configurationFilePath
            ]
            targets
    ,target "test" test
    ,target "retest" $
        makeCleanTarget
            ["tests/haskell/objects"
            ,"tests/haskell/digest-cache"
            ,"tests/haskell/haskell-interfaces"
            ]
        `thenTarget`
        toTarget test
    ]
-- @+node:gcross.20091130053756.1979:configure
configure :: Either ErrorMessage Configuration
configure = parseCommandLineOptions options >>= \(_,options) -> runConfigurer "VMPS.cfg" options $ do
    configurations@
        (ghc_configuration
        ,gcc_configuration
        ,gfortran_configuration
        ,ar_configuration
        ,ld_configuration
        ,install_configuration
        ) <- (,,,,,)
            <$> (configureUsingSection "GHC")
            <*> (configureUsingSection "GCC")
            <*> (configureUsingSection "GCC")
            <*> (configureUsingSection "Binutils")
            <*> (configureUsingSection "Binutils")
            <*> (configureUsingSection "Installation Directories")
    package_dependencies <- configurePackageResolutions ghc_configuration package_description "GHC"
    package_modules <- configurePackageModules ghc_configuration package_dependencies "ZZZ - Please do not edit this unless you know what you are doing."
    return $
        Configuration
            ghc_configuration
            gcc_configuration
            gfortran_configuration
            ar_configuration
            ld_configuration
            install_configuration
            package_dependencies
            package_modules
-- @-node:gcross.20091130053756.1979:configure
-- @+node:gcross.20091201134050.1635:test
test = configure >>= \configuration -> do
    let more_package_dependencies =
            ["HUnit"
            ,"QuickCheck"
            ,"test-framework"
            ,"test-framework-hunit"
            ,"test-framework-quickcheck2"
            ]
    more_package_modules <- runConfigurer "VMPS.cfg" noOptions $
        configurePackageModules
            (ghcConfiguration configuration)
            more_package_dependencies
            "ZZZZ - Please do not edit this unless you know what you are doing."
    attemptGetDigests $
        ghcLinkPrograms
            (ghcConfiguration configuration)
            "tests/haskell/digest-cache"
            ("-lblas":"-llapack":"-larpack":"-lgfortran":testing_ghc_flags)
            (packageDependencies configuration ++ more_package_dependencies)
            "tests/haskell"
            [[("test","o"),("core","o"),("core-wrapper","o")]
            ]
        .
        gfortranCompileAll
            (gfortranConfiguration configuration)
            "tests/haskell/digest-cache"
            gfortran_flags
            "tests/haskell/objects"
            "tests/haskell/fortran-interfaces"
        .
        gccCompileAll
            (gccConfiguration configuration)
            "tests/haskell/digest-cache"
            gcc_flags
            "tests/haskell/objects"
        .
        ghcCompileAll
            (ghcConfiguration configuration)
            "tests/haskell/digest-cache"
            testing_ghc_flags
            (packageModules configuration `Set.union` more_package_modules)
            "tests/haskell/objects"
            "tests/haskell/haskell-interfaces"
        .
        addResources (sourceResourcesIn ("tests" </> "haskell"))
        $
        source_resources
    unsafePerformIO (system "tests/haskell/test") `pseq` return ()
-- @-node:gcross.20091201134050.1635:test
-- @+node:gcross.20091130053756.1980:build
build = configure >>= \configuration ->
    let compiled_resources = 
            gfortranCompileAll
                (gfortranConfiguration configuration)
                "digest-cache"
                gfortran_flags
                "objects"
                "fortran-interfaces"
            .
            gccCompileAll
                (gccConfiguration configuration)
                "digest-cache"
                gcc_flags
                "objects"
            .
            ghcCompileAll
                (ghcConfiguration configuration)
                "digest-cache"
                ghc_flags
                (packageModules configuration)
                "objects"
                "haskell-interfaces"
            $
            source_resources
        object_resources = selectResourcesOfTypeAsList "o" compiled_resources
        library = formStaticLibrary
            (arConfiguration configuration)
            "digest-cache"
            object_resources
            "libvmps"
            "libraries/libvmps.a"
        ghci_library = linkIntoObject
            (ldConfiguration configuration)
            "digest-cache"
            object_resources
            "vmps"
            "libraries/vmps.o"
    in do
        attemptGetDigests [library,ghci_library]
        >>
        return (library,ghci_library,compiled_resources)
-- @-node:gcross.20091130053756.1980:build
-- @+node:gcross.20091130053756.1982:install
install = do
    configuration <- configure
    (library_resource,ghci_library_resource,compiled_resources) <- build
    let interface_resources = selectResourcesOfTypeAsList "hi" compiled_resources
        installation_result =
            installSimplePackage
                (ghcConfiguration configuration)
                (installerConfiguration configuration)
                package_description
                (packageDependencies configuration)
                (library_resource
                 :ghci_library_resource
                 :interface_resources
                )
    case installation_result of
        Nothing -> Right ()
        Just error_message -> Left error_message
-- @-node:gcross.20091130053756.1982:install
-- @-node:gcross.20091130053756.1978:Targets
-- @+node:gcross.20091130053756.1984:main
main = defaultMain
        (createDefaultHelpMessage options . map fst $ targets)
        targets
-- @-node:gcross.20091130053756.1984:main
-- @-others
-- @-node:gcross.20091130053756.1966:@thin Setup.hs
-- @-leo
