#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2419: * @file CMakeLists.txt
add_subdirectory ("common")
add_subdirectory ("c-interface")
add_subdirectory ("programs")

set(PROTO_INCLUDE ${PROTO_INCLUDE} PARENT_SCOPE)
#@-leo
