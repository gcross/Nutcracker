#@+leo-ver=5-thin
#@+node:gcross.20110125120748.2426: * @file CMakeLists.txt
#@+<< Functions >>
#@+node:gcross.20110307093706.2981: ** << Functions >>
function(addAllTestsInDirectory test-directory-name)
    #message(STATUS "Adding directory " ${test-directory-name})
    string(REGEX REPLACE "/" "." test-group-name ${test-directory-name})

    file(GLOB test-source-names ${test-directory-name}/*.cpp)
    foreach(test-source-name ${test-source-names})
        get_filename_component(test-program-name ${test-source-name} NAME_WE)
        set(test-name "${test-group-name}.${test-program-name}")
        add_executable(${test-name} EXCLUDE_FROM_ALL ${test-directory-name}/${test-program-name}.cpp)
        add_dependencies(tests ${test-name})
        target_link_libraries(${test-name} Nutcracker Nutcracker-testing)
        set_property(TARGET ${test-name} PROPERTY RUNTIME_OUTPUT_DIRECTORY bin)
        add_test(${test-name} bin/${test-name})
        #message(STATUS "Adding test " ${test-name})
    endforeach(test-source-name)

    file(GLOB possible-subdirectories ${test-directory-name}/*)
    foreach(possible-subdirectory ${possible-subdirectories})
        #message(STATUS "Checking " ${possible-subdirectory})
        get_filename_component(possible-subdirectory-name ${possible-subdirectory} NAME_WE)
        if(IS_DIRECTORY ${possible-subdirectory} AND NOT ${possible-subdirectory-name} MATCHES "CMakeFiles|\\.\\z")
            #message(STATUS "Scanning " ${possible-subdirectory-name})
            addAllTestsInDirectory (${test-directory-name}/${possible-subdirectory-name})
        endif(IS_DIRECTORY ${possible-subdirectory} AND NOT ${possible-subdirectory-name} MATCHES "CMakeFiles|\\.\\z")
    endforeach(possible-subdirectory)
endfunction(addAllTestsInDirectory)
#@-<< Functions >>

add_custom_target(tests)

add_library(Nutcracker-testing EXCLUDE_FROM_ALL
    test_utils
)
target_link_libraries(Nutcracker-testing
    Nutcracker
)
add_dependencies(tests Nutcracker-testing)

include_directories(".")

addAllTestsInDirectory ("boundaries")
addAllTestsInDirectory ("chain")
addAllTestsInDirectory ("io")
addAllTestsInDirectory ("operators")
addAllTestsInDirectory ("optimizer")
addAllTestsInDirectory ("projectors")
addAllTestsInDirectory ("states")
addAllTestsInDirectory ("tensors")
addAllTestsInDirectory ("utilities")
#@-leo
