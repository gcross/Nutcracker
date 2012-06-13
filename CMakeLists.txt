#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2417: * @file CMakeLists.txt
cmake_minimum_required (VERSION 2.8.8)
project (Nutcracker C CXX Fortran)

set(VERSION 1)

#@+<< Settings >>
#@+node:gcross.20110426132142.2444: ** << Settings >>
#@+others
#@+node:gcross.20110802111539.2331: *3* Bundle required shared libraries
set (BUNDLE_REQUIRED_SHARED_LIBRARIES
    NO
    CACHE BOOL
    "If true, then required shared libraries are bundled with the package."
    )
#@+node:gcross.20110531152523.3452: *3* Output paths
set (LIBRARY_OUTPUT_PATH
    ${PROJECT_BINARY_DIR}/lib
    CACHE PATH
    "Single directory for all libraries."
    )

set (EXECUTABLE_OUTPUT_PATH
    ${PROJECT_BINARY_DIR}/bin
    CACHE PATH
    "Single directory for all executables."
    )

mark_as_advanced(
    LIBRARY_OUTPUT_PATH
    EXECUTABLE_OUTPUT_PATH
    )
#@+node:gcross.20110531152523.3453: *3* Offline HTML+
set (OFFLINE_HTML
    NO
    CACHE BOOL
    "If true, then ensures that HTML documentation can be viewed offline."
    )

if(${OFFLINE_HTML})
    set(USE_MATHJAX NO)
else(${OFFLINE_HTML})
    set(USE_MATHJAX YES)
endif(${OFFLINE_HTML})
#@-others
#@-<< Settings >>

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMakeModules/")

#@+<< Platform-specific tweaks >>
#@+node:gcross.20110427003658.2447: ** << Platform-specific tweaks >>
if(CMAKE_COMPILER_IS_GNUCC)
    add_definitions(-Wall -Wno-strict-aliasing)
endif(CMAKE_COMPILER_IS_GNUCC)
#@-<< Platform-specific tweaks >>

#@+<< Find packages >>
#@+node:gcross.20110426132142.2438: ** << Find packages >>
#@+others
#@+node:gcross.20110426132142.2442: *3* ARPACK
find_package( ARPACK REQUIRED )
link_directories ( ${ARPACK_LIBRARY_DIRS} )
#@+node:gcross.20110426132142.2440: *3* BLAS
find_package( BLAS REQUIRED )
link_directories ( ${BLAS_LIBRARY_DIRS} )
include_directories ( ${BLAS_INCLUDE_DIRS} )
#@+node:gcross.20110426132142.2439: *3* Boost
SET(Boost_ADDITIONAL_VERSIONS "1.43" "1.43.0" "1.44" "1.44.0" "1.45" "1.45.0" "1.46.0" "1.46.1")
find_package( Boost 1.46 COMPONENTS filesystem program_options signals system thread REQUIRED )
link_directories ( ${Boost_LIBRARY_DIRS} )
include_directories ( ${Boost_INCLUDE_DIRS} )
#@+node:gcross.20110910153757.2916: *3* Doxygen & Inkscape
find_package(Doxygen)
find_program(INKSCAPE inkscape)
#@+node:gcross.20110524225044.2475: *3* HDF++
find_package( HDF++ 0.3 REQUIRED )
include_directories ( ${HDF++_INCLUDE_DIRS} )
link_directories ( ${HDF++_LIBRARY_DIRS} )
#@+node:gcross.20110426132142.2441: *3* LAPACK
find_package( LAPACK REQUIRED )
link_directories ( ${LAPACK_LIBRARY_DIRS} )
include_directories ( ${LAPACK_INCLUDE_DIRS} )
#@+node:gcross.20110831211534.2642: *3* Protobuf
find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})
#@+node:gcross.20110426132142.2443: *3* YamlCXX
find_package( YamlCXX 0.2.6 REQUIRED )
link_directories ( ${YAMLCXX_LIBRARY_DIRS} )
include_directories ( ${YAMLCXX_INCLUDE_DIRS} )
#@-others
#@-<< Find packages >>

#@+<< Configure build type >>
#@+node:gcross.20110427003658.2448: ** << Configure build type >>
if( NOT CMAKE_BUILD_TYPE )
set( CMAKE_BUILD_TYPE "RelWithDebInfo" )
endif ()

message (STATUS "Build type is " ${CMAKE_BUILD_TYPE})
#@-<< Configure build type >>

include_directories ("includes")

add_subdirectory ("configuration")
include_directories (${CMAKE_BINARY_DIR}/configuration)

add_subdirectory ("sources")

#@+<< Configure tests >>
#@+node:gcross.20110624000550.2294: ** << Configure tests >>
find_package( Illuminate )

if(${Illuminate_FOUND})
    add_subdirectory ("tests")
else(${Illuminate_FOUND})
    add_custom_target(test
        COMMAND ${CMAKE_COMMAND} -E echo "The Illuminate library is required to build the tests."
    )
endif(${Illuminate_FOUND})
#@-<< Configure tests >>
#@-leo
