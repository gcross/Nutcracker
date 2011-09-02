#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2421: * @file CMakeLists.txt
PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS nutcracker.proto)
set(PROTO_HDRS ${PROTO_HDRS} PARENT_SCOPE)

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
    ${PROTO_HDRS}
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
