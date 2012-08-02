# - Try to find YamlCXX
# Once done this will define
#  YAMLCXX_FOUND - System has YamlCXX
#  YAMLCXX_INCLUDE_DIRS - The YamlCXX include directories
#  YAMLCXX_LIBRARIES - The libraries needed to use YamlCXX
#  YAMLCXX_DEFINITIONS - Compiler switches required for using YamlCXX

find_package(PkgConfig)
pkg_check_modules(PC_YAMLCXX QUIET yaml-cpp)
set(YAMLCXX_DEFINITIONS ${PC_YAMLCXX_CFLAGS_OTHER})

find_path(YAMLCXX_INCLUDE_DIR yaml-cpp/yaml.h
          HINTS ${PC_YAMLCXX_INCLUDEDIR} ${PC_YAMLCXX_INCLUDE_DIRS}
          PATH_SUFFIXES yaml-cpp )

find_library(YAMLCXX_LIBRARY NAMES yaml-cpp libyaml-cpp
             HINTS ${PC_YAMLCXX_LIBDIR} ${PC_YAMLCXX_LIBRARY_DIRS} )

set(YAMLCXX_LIBRARIES ${YAMLCXX_LIBRARY} )
set(YAMLCXX_INCLUDE_DIRS ${YAMLCXX_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set YAMLCXX_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(YamlCXX  DEFAULT_MSG
                                  YAMLCXX_LIBRARY YAMLCXX_INCLUDE_DIR)

mark_as_advanced(YAMLCXX_INCLUDE_DIR YAMLCXX_LIBRARY )
