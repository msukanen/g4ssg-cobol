cmake_minimum_required(VERSION 3.10)
project(G4SSG)

enable_testing()

set(CMAKE_CURRENT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/bin")

set(COBOL_COMPILER cobc)
set(COBOL_FLAGS -x -Icbl/cpy)
set(COBOL_DEBUG )

set(MAIN_SOURCE WRAPPER.cbl
    cbl/MAIN.cbl
    cbl/rng/*.cbl
    cbl/D*.cbl
    cbl/O*.cbl
)
set(CPY_DEPS cbl/cpy/*.cpy)
set(MAIN_EXE g4ssg)
set(MAIN_PATH ${CMAKE_CURRENT_BINARY_DIR}/${MAIN_EXE})

#
# Main App.
#
add_custom_command(
    OUTPUT ${MAIN_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${MAIN_PATH} ${MAIN_SOURCE}
    DEPENDS ${MAIN_SOURCE} ${CPY_DEPS}
    COMMENT "Compiling main application (${MAIN_EXE}) ..."
)
add_custom_target(build_cobol ALL DEPENDS ${MAIN_PATH})

#
# DLUM testing.
#
set(TEST_DLUM_SRC test/DLUM.cbl cbl/DLUM.cbl cbl/rng/ALTERVA?.cbl)
set(TEST_DLUM_EXE tdlum)
set(TEST_DLUM_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_DLUM_EXE})
add_custom_command(
    OUTPUT ${TEST_DLUM_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_DLUM_PATH} ${TEST_DLUM_SRC}
    DEPENDS ${TEST_DLUM_SRC} ${CPY_DEPS}
    COMMENT "Compiling test: ${TEST_DLUM_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_DLUM_EXE} ALL DEPENDS ${TEST_DLUM_PATH})
add_test(NAME TestDLum COMMAND ${TEST_DLUM_PATH})

#
# DSEQ testing.
#
set(TEST_DSEQ_SRC test/DSEQ.cbl cbl/DSEQ.cbl cbl/rng/ALTERVA?.cbl)
set(TEST_DSEQ_EXE tdseq)
set(TEST_DSEQ_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_DSEQ_EXE})
add_custom_command(
    OUTPUT ${TEST_DSEQ_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_DSEQ_PATH} ${TEST_DSEQ_SRC}
    DEPENDS ${TEST_DSEQ_SRC} ${CPY_DEPS}
    COMMENT "Compiling test: ${TEST_DSEQ_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_DSEQ_EXE} ALL DEPENDS ${TEST_DSEQ_PATH})
add_test(NAME TestDSeq COMMAND ${TEST_DSEQ_PATH})

#
# OECC testing.
#
set(TEST_OECC_SRC test/OECC.cbl cbl/OECC.cbl cbl/rng/1D6.cbl cbl/rng/3D6.cbl)
set(TEST_OECC_EXE toecc)
set(TEST_OECC_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_OECC_EXE})
add_custom_command(
    OUTPUT ${TEST_OECC_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_OECC_PATH} ${TEST_OECC_SRC}
    DEPENDS ${TEST_OECC_SRC} ${CPY_DEPS}
    COMMENT "Compiling test: ${TEST_OECC_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_OECC_EXE} ALL DEPENDS ${TEST_OECC_PATH})
add_test(NAME TestOEcc COMMAND ${TEST_OECC_PATH})

#
# OSEP testing.
#
set(TEST_OSEP_SRC test/OSEP.cbl cbl/OSEP.cbl cbl/OECC.cbl cbl/rng/?D6.cbl cbl/rng/ALTERVA?.cbl)
set(TEST_OSEP_EXE tosep)
set(TEST_OSEP_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_OSEP_EXE})
add_custom_command(
    OUTPUT ${TEST_OSEP_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_OSEP_PATH} ${TEST_OSEP_SRC}
    DEPENDS ${TEST_OSEP_SRC} ${CPY_DEPS}
    COMMENT "Compiling test: ${TEST_OSEP_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_OSEP_EXE} ALL DEPENDS ${TEST_OSEP_PATH})
add_test(NAME TestOSep COMMAND ${TEST_OSEP_PATH})

#
# ORBZ testing.
#
set(TEST_ORBZ_SRC test/ORBZ.cbl cbl/ORBZ.cbl)
set(TEST_ORBZ_EXE torbz)
set(TEST_ORBZ_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_ORBZ_EXE})
add_custom_command(
    OUTPUT ${TEST_ORBZ_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_ORBZ_PATH} ${TEST_ORBZ_SRC}
    DEPENDS ${TEST_ORBZ_SRC} ${CPY_DEPS}
    COMMENT "Compiling test: ${TEST_ORBZ_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_ORBZ_EXE} ALL DEPENDS ${TEST_ORBZ_PATH})
add_test(NAME TestOZone COMMAND ${TEST_ORBZ_PATH})
