cmake_minimum_required (VERSION 2.8.8)
project (Nutcracker C CXX Fortran)

set(VERSION 1)

# Settings {{{
# Bundle required shared libraries {{{
set (BUNDLE_REQUIRED_SHARED_LIBRARIES
    NO
    CACHE BOOL
    "If true, then required shared libraries are bundled with the package."
    )
# }}}
# Output paths {{{
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
# }}}
# Offline HTML+ {{{
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
# }}}
# }}}

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMakeModules/")

# Platform-specific tweaks {{{
if(CMAKE_COMPILER_IS_GNUCC)
    add_definitions(-Wall -Wno-strict-aliasing)
endif(CMAKE_COMPILER_IS_GNUCC)
# }}}

# Find packages {{{
# ARPACK {{{
find_package( ARPACK REQUIRED )
link_directories ( ${ARPACK_LIBRARY_DIRS} )
# }}}
# BLAS {{{
find_package( BLAS REQUIRED )
link_directories ( ${BLAS_LIBRARY_DIRS} )
include_directories ( ${BLAS_INCLUDE_DIRS} )
# }}}
# Boost {{{
find_package( Boost 1.48 COMPONENTS filesystem program_options signals system thread REQUIRED )
link_directories ( ${Boost_LIBRARY_DIRS} )
include_directories ( ${Boost_INCLUDE_DIRS} )
# }}}
# Doxygen & Inkscape {{{
find_package(Doxygen)
find_program(INKSCAPE inkscape)
# }}}
# HDF++ {{{
find_package( HDF++ 0.3 REQUIRED )
include_directories ( ${HDF++_INCLUDE_DIRS} )
link_directories ( ${HDF++_LIBRARY_DIRS} )
# }}}
# LAPACK {{{
find_package( LAPACK REQUIRED )
link_directories ( ${LAPACK_LIBRARY_DIRS} )
include_directories ( ${LAPACK_INCLUDE_DIRS} )
# }}}
# Protobuf {{{
find_package(Protobuf REQUIRED)
include_directories(${PROTOBUF_INCLUDE_DIRS})
# }}}
# YamlCXX {{{
find_package( YamlCXX 0.5.0 REQUIRED )
link_directories ( ${YAMLCXX_LIBRARY_DIRS} )
include_directories ( ${YAMLCXX_INCLUDE_DIRS} )
# }}}
# }}}

# Configure build type {{{
if( NOT CMAKE_BUILD_TYPE )
set( CMAKE_BUILD_TYPE "RelWithDebInfo" )
endif ()

message (STATUS "Build type is " ${CMAKE_BUILD_TYPE})
# }}}

include_directories ("includes")

add_subdirectory ("configuration")
include_directories (${CMAKE_BINARY_DIR}/configuration)

add_subdirectory ("sources")

# Configure tests {{{
find_package( Illuminate )

if(${Illuminate_FOUND})
    add_subdirectory ("tests")
else(${Illuminate_FOUND})
    add_custom_target(test
        COMMAND ${CMAKE_COMMAND} -E echo "The Illuminate library is required to build the tests."
    )
endif(${Illuminate_FOUND})
# }}}
