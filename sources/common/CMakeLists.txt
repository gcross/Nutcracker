#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2421: * @thin CMakeLists.txt
add_library(Nutcracker
    core.f95
    core.cpp
    tensors.cpp
    utilities.cpp
)
target_link_libraries(Nutcracker
    ${Boost_LIBRARIES}
    ${BLAS_LIBRARIES}
    ${LAPACK_LIBRARIES}
    ${ARPACK_LIBRARIES}
)
#@-leo
