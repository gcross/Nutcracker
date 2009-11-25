-- @+leo-ver=4-thin
-- @+node:gcross.20091123113033.1620:@thin Setup.hs
-- @@language Haskell

import Data.Maybe
import qualified Data.Map as Map

import Blueprint.Resources
import Blueprint.Tools.Ar
import Blueprint.Tools.GCC
import Blueprint.Tools.GHC

flags = ["-O3","-ffast-math","-funroll-loops"]
cflags = flags
fflags = "-cpp":"-fimplicit-none":flags
hflags = ["-O2","-fvia-C","-optc=-O3"]

main = do
    let src_resources = resourcesIn "src"
        gcc_tools = fromJust gccTools
        ghc_tools = fromJust ghcTools
        Right package_modules = getPackages ghc_tools
            ["base"
            ,"mtl"
            ,"array"
            ,"bytestring"
            ,"InfixApplicative"
            ]
        compiled_resources =
            gccCompileAllFortran
                gcc_tools
                fflags
                "objects"
                "fortran-interfaces"
                "digest-cache"
            .
            gccCompileAllC
                gcc_tools
                cflags
                "objects"
                "digest-cache"
            .
            ghcCompileAll
                ghc_tools
                hflags
                package_modules
                "objects"
                "haskell-interfaces"
                "digest-cache"
            $
            src_resources
        library = formStaticLibrary
            (fromJust arTools)
            "hash-cache"
            (map snd . filter ((=="o") . snd . fst) . Map.toList  $ compiled_resources)
            "libvmps"
            "lib/libvmps.a"
    case resourceDigest library of
        Left error_message -> putStrLn $ makeCompositeErrorMessage error_message
        Right digest -> putStrLn "Done!"
-- @-node:gcross.20091123113033.1620:@thin Setup.hs
-- @-leo
