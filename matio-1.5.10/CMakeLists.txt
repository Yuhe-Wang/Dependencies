cmake_minimum_required (VERSION 2.8)

set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR}/../)

project(libmatio C)

aux_source_directory(./src DIR_SRCS)

include_directories(${PROJECT_SOURCE_DIR}/visual_studio)

include_directories(${PROJECT_BINARY_DIR}/../include/zlib)

include_directories(${PROJECT_BINARY_DIR}/../include/hdf5)

add_definitions(
-DREPLACE_GETOPT
-DMAT73=1
-DHAVE_HDF5=1
-DHAVE_ZLIB=1
-DH5_BUILT_AS_DYNAMIC_LIB
-DH5_NO_DEPRECATED_SYMBOLS
)

link_directories(${PROJECT_BINARY_DIR}/../lib) # should be placed before add_library

add_library(matio SHARED ${DIR_SRCS})

target_link_libraries(matio hdf5 zlib)

if(WIN32)
    SET_TARGET_PROPERTIES(matio PROPERTIES LINK_FLAGS /DEF:"${PROJECT_SOURCE_DIR}/visual_studio/matio.def")
	install(FILES "${PROJECT_BINARY_DIR}/matio.dll" DESTINATION "${PROJECT_BINARY_DIR}/../bin")
	install(FILES "${PROJECT_BINARY_DIR}/matio.lib" DESTINATION "${PROJECT_BINARY_DIR}/../lib")
	install(FILES "${PROJECT_SOURCE_DIR}/visual_studio/matio_pubconf.h" DESTINATION "${PROJECT_BINARY_DIR}/../include/matio")
	file(GLOB headers "${PROJECT_SOURCE_DIR}/src/*.h")
	install(FILES ${headers} DESTINATION "${PROJECT_BINARY_DIR}/../include/matio")
endif(WIN32)
