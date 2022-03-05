
cmake_minimum_required(VERSION 3.14)

project(ospraye VERSION 1.0)

option(INSTALL_IN_PLACE "Install to 'PWD'/ebin and 'PWD'/priv" ON)
option(INSTALL_DEP_LIBS "Install dependencies libs to priv" ON)

find_program(ERL_EXEC erl)
find_program(ERLC_EXEC erlc)

if(NOT ERL_EXEC)
  message(FATAL_ERROR "Erlang not found add it to the path: '${ERL_EXEC}'")
endif()

if(INSTALL_IN_PLACE)
  set(CMAKE_INSTALL_PREFIX "${PROJECT_SOURCE_DIR}")
else()
  set(CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}/${PROJECT_NAME}-${CMAKE_PROJECT_VERSION}")
endif()

add_subdirectory(src)
add_subdirectory(c_src)
