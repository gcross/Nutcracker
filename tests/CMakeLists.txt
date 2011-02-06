#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2426: * @thin CMakeLists.txt
include_directories(".")

set(test-SOURCES
    chain
    core
    tensors
    test_utils
)

add_executable(test EXCLUDE_FROM_ALL ${test-SOURCES})
target_link_libraries(test Nutcracker illuminate-main illuminate)
set_property(TARGET test PROPERTY RUNTIME_OUTPUT_DIRECTORY .)
#@-leo
