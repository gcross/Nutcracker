#@+leo-ver=5-thin
#@+node:gcross.20110220141808.2949: * @thin CMakeLists.txt
add_executable(nutcracker
    nutcracker
)
target_link_libraries(nutcracker Nutcracker)
install (TARGETS nutcracker RUNTIME DESTINATION bin)
#@-leo
