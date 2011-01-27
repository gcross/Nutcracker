#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2426: * @thin CMakeLists.txt
include_directories(".")

set(test-SOURCES
    core
)

add_executable(test EXCLUDE_FROM_ALL ${test-SOURCES})
target_link_libraries(test Nutcracker illuminate-main illuminate ${Boost_LIBRARIES})
set_property(TARGET test PROPERTY RUNTIME_OUTPUT_DIRECTORY .)
#@-leo
