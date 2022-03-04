##
##

cmake_minimum_required(VERSION 3.1)

project(ospraye)

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  message(FATAL_ERROR "OSPRay can only be built on 64-bit platforms. This
  problem most commonly happens on Windows, where the right solution is to
  switch to a 64-bit generator: retry running CMake with '-G \"Visual
  Studio 15 2017 Win64\"'")
endif()

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

# We want the installed target lib in priv

string(REGEX MATCH "^.*/" CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}")
string(APPEND CMAKE_INSTALL_PREFIX "priv")

find_program(ERL_EXEC erl)

if(NOT ERL_EXEC)
  message(FATAL_ERROR "Erlang not found add it to the path: '${ERL_EXEC}'")
endif()

EXECUTE_PROCESS (COMMAND
  ${ERL_EXEC} -noshell -eval "io:fwrite(\"~s\",[code:root_dir()]),init:stop()."
  OUTPUT_VARIABLE ERL_ROOT)

set(ERL_INC "${ERL_ROOT}/usr/include")

find_package(ospray 2.8.0 REQUIRED)

add_library(osp SHARED osp_nif.cpp)

target_include_directories(osp PUBLIC ${ERL_INC})
target_link_libraries(osp ospray::ospray)

set_target_properties(osp PROPERTIES RUNTIME_OUTPUT_NAME libosp)

set(installable_libs osp)

if(WIN32)
  install(TARGETS ${installable_libs} RUNTIME DESTINATION "${CMAKE_INSTALL_PREFIX}")
else()
  install(TARGETS ${installable_libs} LIBRARY DESTINATION "${CMAKE_INSTALL_PREFIX}")
endif()

get_target_property(OSPRAY_LIBNAME ospray::ospray LOCATION)
string(REGEX MATCH "^.*/" _sharedlib_glob "${OSPRAY_LIBNAME}")
string(APPEND _sharedlib_glob "*${CMAKE_SHARED_LIBRARY_SUFFIX}*")
file(GLOB _sharedlibs LIST_DIRECTORIES false "${_sharedlib_glob}")
install(FILES ${_sharedlibs} DESTINATION "${CMAKE_INSTALL_PREFIX}")

