#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2417: * @thin CMakeLists.txt
cmake_minimum_required (VERSION 2.8)
project (Nutcracker C CXX Fortran)

enable_testing()

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMakeModules/")

SET(Boost_ADDITIONAL_VERSIONS "1.43" "1.43.0" "1.44" "1.44.0" "1.45" "1.45.0")

find_package( Boost 1.45 COMPONENTS thread program_options REQUIRED )
link_directories ( ${Boost_LIBRARY_DIRS} )
include_directories ( ${Boost_INCLUDE_DIRS} )

if(APPLE)
    message (STATUS "Compiling on Darwin, adding -arch x86_64")
    add_definitions(-arch x86_64)
endif(APPLE)

find_package( BLAS REQUIRED )
link_directories ( ${BLAS_LIBRARY_DIRS} )
include_directories ( ${BLAS_INCLUDE_DIRS} )

find_package( LAPACK REQUIRED )
link_directories ( ${LAPACK_LIBRARY_DIRS} )
include_directories ( ${LAPACK_INCLUDE_DIRS} )

find_package( ARPACK REQUIRED )
link_directories ( ${ARPACK_LIBRARY_DIRS} )

find_package( YamlCXX REQUIRED )
link_directories ( ${YAMLCXX_LIBRARY_DIRS} )
include_directories ( ${YAMLCXX_INCLUDE_DIRS} )

if(UNIX)
    if(CMAKE_COMPILER_IS_GNUCC)
         add_definitions(-Wall -Wno-strict-aliasing)
    endif(CMAKE_COMPILER_IS_GNUCC)
endif(UNIX)

if( NOT CMAKE_BUILD_TYPE )
set( CMAKE_BUILD_TYPE "RelWithDebInfo" )
endif ()

message (STATUS "Build type is " ${CMAKE_BUILD_TYPE})

include_directories ("includes")

add_subdirectory ("sources")
add_subdirectory ("tests")
#@-leo
