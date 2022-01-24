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

# control where the static and shared libraries are built so that on windows
# we don't need to tinker with the path to run the executable
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "priv")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "priv")

find_program(ERL_EXEC erl)

EXECUTE_PROCESS (COMMAND
  ${ERL_EXEC} -noshell -eval "io:fwrite(\"~s\",[code:root_dir()]),init:stop()."
  OUTPUT_VARIABLE ERL_ROOT)

set(ERL_INC "${ERL_ROOT}/usr/include")

find_package(ospray 2.8.0 REQUIRED)

add_library(osp MODULE osp_nif.cpp)

target_include_directories(osp PUBLIC ${ERL_INC})
#target_include_directories(osp PUBLIC ospray::ospray)
target_link_libraries(osp ospray::ospray)


set(installable_libs osp)
install(TARGETS ${installable_libs} DESTINATION ../priv)
