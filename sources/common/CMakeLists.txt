PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS nutcracker.proto)
get_filename_component(PROTO_INCLUDE ${PROTO_HDRS} PATH)
set(PROTO_INCLUDE ${PROTO_INCLUDE} PARENT_SCOPE)

include_directories (${PROTO_INCLUDE})

add_library(Nutcracker++ SHARED
    base_chain
    boundaries
    chain
    chain_options
    compiler
    configuration
    core.f95
    core.cpp
    flat
    hdf
    infinite_chain
    infinite_operators
    io
    operators
    optimizer
    projectors
    protobuf
    states
    tensors
    utilities
    version
    yaml

    ${PROTO_SRCS}
)
target_link_libraries(Nutcracker++
    ${Boost_LIBRARIES}
    ${BLAS_LIBRARIES}
    ${LAPACK_LIBRARIES}
    ${ARPACK_LIBRARIES} # Note that ARPACK *MUST* come after LAPACK!
    ${PROTOBUF_LIBRARIES}
    ${YAMLCXX_LIBRARIES}
    HDF++
)
install (TARGETS Nutcracker++ LIBRARY DESTINATION lib)
