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
import Data.Maybe
import qualified Data.Map as Map
import Data.Version

import Distribution.Package
import qualified Distribution.PackageDescription as Package

import System.Directory
import System.IO.Unsafe

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Blueprint.Configuration
import Blueprint.Error
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
ghc_flags = ["-O2","-fvia-C","-optc=-O3","-package-name="++qualified_package_name]

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
    }
-- @-node:gcross.20091130053756.1972:Configuration
-- @-node:gcross.20091130053756.1971:Types
-- @+node:gcross.20091130053756.1973:Values
-- @+node:gcross.20091130053756.1974:source resources
source_resources = resourcesIn "sources"
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
            ]
    ,target "distclean" $
        makeDistCleanTarget
            [configurationFilePath
            ]
            targets
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
    return $
        Configuration
            ghc_configuration
            gcc_configuration
            gfortran_configuration
            ar_configuration
            ld_configuration
            install_configuration
            package_dependencies
-- @-node:gcross.20091130053756.1979:configure
-- @+node:gcross.20091130053756.1980:build
build = configure >>= \configuration ->
    let Right package_modules = getPackages <$> ghcConfiguration <*> packageDependencies $ configuration
        compiled_resources = 
            gfortranCompileAll
                (gfortranConfiguration configuration)
                gfortran_flags
                "objects"
                "fortran-interfaces"
                "digest-cache"
            .
            gccCompileAll
                (gccConfiguration configuration)
                gcc_flags
                "objects"
                "digest-cache"
            .
            ghcCompileAll
                (ghcConfiguration configuration)
                ghc_flags
                package_modules
                "objects"
                "haskell-interfaces"
                "digest-cache"
            $
            source_resources
        object_resources =
            map snd
            .
            filter ((=="o"). snd . fst)
            .
            Map.toList
            $
            compiled_resources
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
    let interface_resources = filter ((=="hi") . resourceType) . Map.elems $ compiled_resources
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
