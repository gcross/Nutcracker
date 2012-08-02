add_executable(nutcracker
    nutcracker
)
target_link_libraries(nutcracker Nutcracker)
install (TARGETS nutcracker RUNTIME DESTINATION bin)

add_executable(print-object-sizes
    print-object-sizes
)
target_link_libraries(print-object-sizes Nutcracker)

if(BUNDLE_REQUIRED_SHARED_LIBRARIES)
    if(UNIX AND NOT APPLE)
        set_target_properties(nutcracker PROPERTIES INSTALL_RPATH "\$ORIGIN/../share/nutcracker-${VERSION}/lib")
    endif()
    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/FixBundle.cmake.in
        ${CMAKE_BINARY_DIR}/miscellaneous/FixBundle.cmake
        @ONLY
    )
    configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/GetPrerequisitesOverrides.cmake.in
        ${CMAKE_BINARY_DIR}/miscellaneous/GetPrerequisitesOverrides.cmake
        @ONLY
    )
    install(SCRIPT ${CMAKE_BINARY_DIR}/miscellaneous/FixBundle.cmake)
endif()
