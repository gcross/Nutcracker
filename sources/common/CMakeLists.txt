#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2421: * @file CMakeLists.txt
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS nutcracker.proto)
get_filename_component(PROTO_INCLUDE ${PROTO_HDRS} PATH)
set(PROTO_INCLUDE ${PROTO_INCLUDE} PARENT_SCOPE)

include_directories (${PROTO_INCLUDE})

add_library(Nutcracker
    boundaries
    chain
    compiler
    configuration
    core.f95
    core.cpp
    flat
    hdf
    io
    operators
    optimizer
    projectors
    protobuf
    states
    tensors
    utilities
    yaml

    ${PROTO_SRCS}
)
target_link_libraries(Nutcracker
    ${Boost_LIBRARIES}
    ${BLAS_LIBRARIES}
    ${LAPACK_LIBRARIES}
    ${ARPACK_LIBRARIES} # Note that ARPACK *MUST* come after LAPACK!
    ${PROTOBUF_LIBRARIES}
    ${YAMLCXX_LIBRARIES}
    HDF++
)
#@-leo
