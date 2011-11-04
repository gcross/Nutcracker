#@+leo-ver=5-thin
#@+node:gcross.20110910153757.2917: * @file CMakeLists.txt
#@+others
#@+node:gcross.20110427003658.2449: ** Doxyfile
if(DOXYGEN_FOUND AND INKSCAPE)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)

    file(GLOB svg-file-paths ${CMAKE_SOURCE_DIR}/documentation/images/*.svg)
    foreach(svg-file-path ${svg-file-paths})
        get_filename_component(image-name ${svg-file-path} NAME_WE)
        set(eps-file-path ${CMAKE_BINARY_DIR}/doc/images/${image-name}.eps)
        set(pdf-file-path ${CMAKE_BINARY_DIR}/doc/images/${image-name}.pdf)
        set(png-file-path ${CMAKE_BINARY_DIR}/doc/images/${image-name}.png)
        add_custom_command(
            OUTPUT ${eps-file-path} ${pdf-file-path} ${png-file-path}
            COMMAND ${CMAKE_COMMAND} -E make_directory ${CMAKE_BINARY_DIR}/doc/images
            COMMAND
                ${INKSCAPE}
                    --without-gui
                    --file=${svg-file-path}
                    --export-eps=${eps-file-path}
                    --export-png=${png-file-path}
                    --export-pdf=${pdf-file-path}
            DEPENDS ${image-file-path}
            COMMENT "Generating EPS/PDF/PNG files for SVG image ${image-name}"
            VERBATIM
        )
        set(doc-image-sources ${doc-image-sources} ${eps-file-path} ${pdf-file-path} ${png-file-path})
    endforeach(svg-file-path)
    add_custom_target(doc-images SOURCES ${doc-image-sources})

    add_custom_target(doc
        COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
        DEPENDS ${doc-image-sources}
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        COMMENT "Generating API documentation with Doxygen"
        VERBATIM
    )

endif(DOXYGEN_FOUND AND INKSCAPE)
#@+node:gcross.20110910164923.2933: ** Header
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/_config.h.in ${CMAKE_CURRENT_BINARY_DIR}/_config.h @ONLY)
#@-others
#@-leo
