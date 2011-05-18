#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2421: * @file CMakeLists.txt
add_library(Nutcracker
    boundaries
    chain
    configuration
    core.f95
    core.cpp
    flat
    hdf
    io
    operators
    optimizer
    projectors
    states
    tensors
    utilities
    yaml
)
target_link_libraries(Nutcracker
    ${Boost_LIBRARIES}
    ${BLAS_LIBRARIES}
    ${HDF5_LIBRARIES}
    ${LAPACK_LIBRARIES}
    ${ARPACK_LIBRARIES} # Note that ARPACK *MUST* come after LAPACK!
    ${YAMLCXX_LIBRARIES}
)
#@-leo
