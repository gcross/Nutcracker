include_directories(".")
include_directories(${PROTO_INCLUDE})
include_directories ( ${Illuminate_INCLUDE_DIRS} )
link_directories ( ${Illuminate_LIBRARY_DIRS} )

set(test-SOURCES
    boundaries
    chain
    c-interface
    compiler
    configuration
    flat
    hdf
    infinite_chain
    io
    optimizer
    projectors
    protobuf
    states
    tensors
    utilities
    yaml

    test_utils
)

add_executable(test EXCLUDE_FROM_ALL ${test-SOURCES})
target_link_libraries(test Nutcracker Nutcracker++ illuminate-main illuminate)
set_property(TARGET test PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})

add_executable(stress EXCLUDE_FROM_ALL stress test_utils)
target_link_libraries(stress Nutcracker illuminate-main illuminate)
set_property(TARGET stress PROPERTY RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR})
