set(SRC "${PROJECT_SOURCE_DIR}/src")
set(EBIN "${PROJECT_SOURCE_DIR}/ebin")

set(ERL_FILES osp)
set(HRL_FILES ${SRC}/osp_types.hrl)
set(ERL_COMPILE_FLAGS -Werror +debug_info)

# Compile erlang files
foreach(ERL_FILE ${ERL_FILES})
  add_custom_command(
    OUTPUT  ${EBIN}/${ERL_FILE}.beam
    DEPENDS ${ERL_FILE}.erl ${HRL_FILES}
    COMMENT ${ERLC_EXEC} ${ERL_COMPILE_FLAGS} -o${EBIN} ${SRC}/${ERL_FILE}.erl
    COMMAND ${ERLC_EXEC} ${ERL_COMPILE_FLAGS} -o${EBIN} ${SRC}/${ERL_FILE}.erl)
  list(APPEND BEAM_FILES ${EBIN}/${ERL_FILE}.beam)
endforeach()

# Patch in version to app.src file
file(READ "${SRC}/${PROJECT_NAME}.app.src" APPSRC)
string(REPLACE "@VERSION@" "\"${CMAKE_PROJECT_VERSION}\"" APPSRC_PATCHED ${APPSRC})
file(WRITE ${EBIN}/${PROJECT_NAME}.app ${APPSRC_PATCHED})
list(APPEND BEAM_FILES ${EBIN}/${PROJECT_NAME}.app)

add_custom_target(beam ALL DEPENDS ${BEAM_FILES})
install(FILES ${BEAM_FILES} DESTINATION "${CMAKE_INSTALL_PREFIX}/ebin")
