#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2426: * @file CMakeLists.txt
include_directories(".")
include_directories ( ${Illuminate_INCLUDE_DIRS} )
link_directories ( ${Illuminate_LIBRARY_DIRS} )

set(test-SOURCES
    boundaries
    chain
    configuration
    flat
    hdf
    io
    optimizer
    projectors
    tensors
    utilities
    yaml

    test_utils
)

add_executable(test EXCLUDE_FROM_ALL ${test-SOURCES})
target_link_libraries(test Nutcracker illuminate-main illuminate)
set_property(TARGET test PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
#@-leo
