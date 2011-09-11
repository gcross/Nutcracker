#@+leo-ver=5-thin
#@+node:gcross.20110823131135.2599: * @file CMakeLists.txt
include_directories(".")

add_library(Nutcracker SHARED
    errors
    matrix
    operator
    operator_builder
    operator_term
    serialization
    state
    state_builder
    state_term
    vector
    version
)
target_link_libraries(Nutcracker Nutcracker++)
#@-leo
