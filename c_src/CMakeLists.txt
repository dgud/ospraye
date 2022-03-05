##
##

cmake_minimum_required(VERSION 3.14)

project(ospraye)

if(CMAKE_SIZEOF_VOID_P EQUAL 4)
  message(FATAL_ERROR "OSPRay can only be built on 64-bit platforms. This
  problem most commonly happens on Windows, where the right solution is to
  switch to a 64-bit generator: retry running CMake with '-G \"Visual
  Studio 15 2017 Win64\"'")
endif()

option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

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
  install(TARGETS ${installable_libs} RUNTIME DESTINATION "${CMAKE_INSTALL_PREFIX}/priv")
else()
  install(TARGETS ${installable_libs} LIBRARY DESTINATION "${CMAKE_INSTALL_PREFIX}/priv")
endif()

if(INSTALL_DEP_LIBS)
  get_target_property(OSPRAY_LIBNAME ospray::ospray LOCATION)
  string(REGEX MATCH "^.*/" _sharedlib_glob "${OSPRAY_LIBNAME}")
  string(APPEND _sharedlib_glob "*${CMAKE_SHARED_LIBRARY_SUFFIX}*")
  file(GLOB _sharedlibs LIST_DIRECTORIES false "${_sharedlib_glob}")
  install(FILES ${_sharedlibs} DESTINATION "${CMAKE_INSTALL_PREFIX}/priv")
endif()

###  DEBUGGING

## Get all properties that cmake supports
execute_process(COMMAND cmake --help-property-list OUTPUT_VARIABLE CMAKE_PROPERTY_LIST)
## Convert command output into a CMake list
STRING(REGEX REPLACE ";" "\\\\;" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
STRING(REGEX REPLACE "\n" ";" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")

list(REMOVE_DUPLICATES CMAKE_PROPERTY_LIST)

function(print_target_properties tgt)
  if(NOT TARGET ${tgt})
    message("There is no target named '${tgt}'")
    return()
  endif()

  foreach (prop ${CMAKE_PROPERTY_LIST})
    string(REPLACE "<CONFIG>" "${CMAKE_BUILD_TYPE}" prop ${prop})
    get_target_property(propval ${tgt} ${prop})
    if (propval)
      message ("${tgt} ${prop} = ${propval}")
    endif()
  endforeach(prop)
endfunction(print_target_properties)

#print_target_properties(osp)
