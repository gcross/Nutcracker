add_subdirectory ("common")

include_directories(${PROTO_INCLUDE})

add_subdirectory ("c-interface")
add_subdirectory ("programs")

set(PROTO_INCLUDE ${PROTO_INCLUDE} PARENT_SCOPE)
