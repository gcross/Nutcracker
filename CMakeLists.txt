#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2417: * @file CMakeLists.txt
cmake_minimum_required (VERSION 2.8)
project (Nutcracker C CXX Fortran)

#@+<< Settings >>
#@+node:gcross.20110426132142.2444: ** << Settings >>
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
#@-<< Settings >>

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMakeModules/")

#@+<< Platform-specific tweaks >>
#@+node:gcross.20110427003658.2447: ** << Platform-specific tweaks >>
if(APPLE)
    message (STATUS "Compiling on Darwin, adding -arch x86_64")
    add_definitions(-arch x86_64)
endif(APPLE)

if(UNIX)
    if(CMAKE_COMPILER_IS_GNUCC)
         add_definitions(-Wall -Wno-strict-aliasing)
    endif(CMAKE_COMPILER_IS_GNUCC)
endif(UNIX)
#@-<< Platform-specific tweaks >>

#@+<< Find packages >>
#@+node:gcross.20110426132142.2438: ** << Find packages >>
#@+others
#@+node:gcross.20110426132142.2439: *3* Boost
SET(Boost_ADDITIONAL_VERSIONS "1.43" "1.43.0" "1.44" "1.44.0" "1.45" "1.45.0")
find_package( Boost 1.45 COMPONENTS thread program_options REQUIRED )
link_directories ( ${Boost_LIBRARY_DIRS} )
include_directories ( ${Boost_INCLUDE_DIRS} )
#@+node:gcross.20110426132142.2440: *3* BLAS
find_package( BLAS REQUIRED )
link_directories ( ${BLAS_LIBRARY_DIRS} )
include_directories ( ${BLAS_INCLUDE_DIRS} )
#@+node:gcross.20110426132142.2441: *3* LAPACK
find_package( LAPACK REQUIRED )
link_directories ( ${LAPACK_LIBRARY_DIRS} )
include_directories ( ${LAPACK_INCLUDE_DIRS} )
#@+node:gcross.20110426132142.2442: *3* ARPACK
find_package( ARPACK REQUIRED )
link_directories ( ${ARPACK_LIBRARY_DIRS} )
#@+node:gcross.20110426132142.2443: *3* YamlCXX
find_package( YamlCXX REQUIRED )
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

#@+<< Configure documentation >>
#@+node:gcross.20110427003658.2449: ** << Configure documentation >>
find_package(Doxygen)
find_program(
    INKSCAPE
    NAMES inkscape-bin inkscape
    PATHS = /Applications/Inkscape.app/Contents/Resources/bin
)

if(DOXYGEN_FOUND AND INKSCAPE)
    configure_file(${CMAKE_SOURCE_DIR}/Doxyfile.in ${CMAKE_BINARY_DIR}/Doxyfile @ONLY)

    file(GLOB svg-file-paths ${CMAKE_SOURCE_DIR}/documentation/images/*.svg)
    foreach(svg-file-path ${svg-file-paths})
        get_filename_component(image-name ${svg-file-path} NAME_WE)
        set(eps-file-path ${CMAKE_BINARY_DIR}/doc/images/${image-name}.eps)
        set(pdf-file-path ${CMAKE_BINARY_DIR}/doc/images/${image-name}.pdf)
        set(png-file-path ${CMAKE_BINARY_DIR}/doc/images/${image-name}.png)
        add_custom_command(
            OUTPUT ${eps-file-path} ${pdf-file-path} ${png-file-path}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/doc/images
            COMMAND
                ${INKSCAPE}
                    --without-gui
                    --file=${svg-file-path}
                    --export-eps=${eps-file-path}
                    --export-png=${png-file-path}
                    --export-pdf=${pdf-file-path}
            DEPENDS ${image-file-path}
            COMMENT "Generating EPS/PDF/PNG files for SVG image ${image-name}"
            VERBATIM
        )
        set(doc-image-sources ${doc-image-sources} ${eps-file-path} ${pdf-file-path} ${png-file-path})
    endforeach(svg-file-path)
    add_custom_target(doc-images SOURCES ${doc-image-sources})

    add_custom_target(doc
        COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_BINARY_DIR}/Doxyfile
        DEPENDS doc-images
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        COMMENT "Generating API documentation with Doxygen"
        VERBATIM
    )

endif(DOXYGEN_FOUND AND INKSCAPE)
#@-<< Configure documentation >>

include_directories ("includes")

add_subdirectory ("sources")
add_subdirectory ("tests")
#@-leo
