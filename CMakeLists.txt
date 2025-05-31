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
)
set(MAIN_EXE g4ssg)
set(MAIN_PATH ${CMAKE_CURRENT_BINARY_DIR}/${MAIN_EXE})

#
# Main App.
#
add_custom_command(
    OUTPUT ${MAIN_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${MAIN_PATH} ${MAIN_SOURCE}
    DEPENDS ${MAIN_SOURCE}
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
    DEPENDS ${TEST_DLUM_SRC}
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
    DEPENDS ${TEST_DSEQ_SRC}
    COMMENT "Compiling test: ${TEST_DSEQ_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_DSEQ_EXE} ALL DEPENDS ${TEST_DSEQ_PATH})
add_test(NAME TestDSeq COMMAND ${TEST_DSEQ_PATH})
