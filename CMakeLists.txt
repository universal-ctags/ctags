cmake_minimum_required(VERSION 3.22)
project(
        readtags
	#
	# Synchronize VERSION with the value in configure.ac.
        VERSION 0.4.0
        DESCRIPTION "a library for looking up tag entries in tag files"
        HOMEPAGE_URL "https://github.com/universal-ctags/libreadtags"
        LANGUAGES C
)

set(CMAKE_C_STANDARD 99)

option(LIBREADTAGS_BUILD_SHARED "Build shared/static library" OFF)

# You don't have to edit API_VERSION.
# See https://cmake.org/cmake/help/latest/command/cmake_file_api.html
set(API_VERSION 1)
#
# Synchronize BUILD_VERSION with the value appended to libreadtags.so.<>.
# that is built with Autotools.
set(BUILD_VERSION 1.1.2)

# Taken from https://gitlab.kitware.com/cmake/community/-/wikis/contrib/macros/TestInline
# Inspired from /usr/share/autoconf/autoconf/c.m4
foreach(KEYWORD "inline" "__inline__" "__inline")
  if(NOT DEFINED C_INLINE)
    try_compile(C_HAS_${KEYWORD} "${CMAKE_CURRENT_BINARY_DIR}"
      "${CMAKE_CURRENT_SOURCE_DIR}/test_inline.c"
      COMPILE_DEFINITIONS "-Dinline=${KEYWORD}")
    if(C_HAS_${KEYWORD})
      SET(C_INLINE TRUE)
      ADD_DEFINITIONS("-Dinline=${KEYWORD}")
    endif(C_HAS_${KEYWORD})
  endif(NOT DEFINED C_INLINE)
endforeach(KEYWORD)
if(NOT DEFINED C_INLINE)
  ADD_DEFINITIONS("-Dinline=")
endif(NOT DEFINED C_INLINE)

if (LIBREADTAGS_BUILD_SHARED)
    message(STATUS "Building shared library")
    add_library(${PROJECT_NAME} SHARED readtags.c)
    set_target_properties(${PROJECT_NAME} PROPERTIES
            VERSION ${BUILD_VERSION}
            SOVERSION ${API_VERSION}
            PUBLIC_HEADER readtags.h
    )
    install(TARGETS ${PROJECT_NAME}
            LIBRARY DESTINATION lib
            PUBLIC_HEADER DESTINATION include
    )
else ()
    message(STATUS "Building static library")
    add_library(${PROJECT_NAME} STATIC readtags.c)
    install(TARGETS ${PROJECT_NAME}
            ARCHIVE DESTINATION lib
            PUBLIC_HEADER DESTINATION include
    )
endif ()

add_library(universal-ctags::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

enable_testing()
set(test_cases
  test-api-tagsOpen
  test-api-tagsFind
  test-api-tagsFindPseudoTag
  test-api-tagsFirstPseudoTag
  test-api-tagsFirst
  test-api-tagsClose
  test-api-tagsSetSortType

  test-fix-unescaping
  test-fix-null-deref
  test-fix-large-tags
  test-fix-unescaping-input-fields
  test-fix-unescaping-input-fields-exuberant
  test-fix-unescaping-input-fields-no-mode
  test-fix-unescaping-input-fields-backslash
  test-fix-unescaping-input-fields-no-filesep
)

foreach(t IN LISTS test_cases)
  add_executable(${t} tests/${t}.c)
  target_link_libraries(${t} readtags)
  add_test(NAME ${t}
    COMMAND ${t}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/tests)
endforeach()

#
# TODO:
#
# * Support find_package
#
#   ParticleG's comments quoted from
#   https://github.com/universal-ctags/libreadtags/pull/51#issuecomment-2148017962
#
#      Actually, if we prepare CMakeList.txt properly, other CMake projects
#      just need to use find_package method to load this package, see
#      Finding Packages. But if you use CMake method FetchContent to add
#      libreadtags to your project, then current changes should be enough.
#
