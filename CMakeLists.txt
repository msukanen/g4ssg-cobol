cmake_minimum_required(VERSION 3.10)
project(G4SSG)

enable_testing()

set(CMAKE_CURRENT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/bin")

set(CPY_DIR cbl/cpy)
set(CPY_DEPS ${CPY_DIR}/*.cpy)
set(RNG_DEPS cbl/rng/?D6.cbl)

set(COBOL_COMPILER cobc)
set(COBOL_FLAGS -x -I${CPY_DIR})
set(COBOL_DEBUG )

#
# Main App.
#
set(MAIN_SOURCE WRAPPER.cbl
    cbl/MAIN.cbl
    ${RNG_DEPS}
    cbl/STRFMT*.cbl
)
set(MAIN_EXE g4ssg)
set(MAIN_PATH ${CMAKE_CURRENT_BINARY_DIR}/${MAIN_EXE})
add_custom_command(
    OUTPUT ${MAIN_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${MAIN_PATH} ${MAIN_SOURCE}
    DEPENDS ${MAIN_SOURCE} ${CPY_DEPS}
    COMMENT "Compiling main application (${MAIN_EXE}) ..."
)
add_custom_target(build_cobol ALL DEPENDS ${MAIN_PATH})

#
# Some foobar string testing.
#
set(TEST_STRFMT_SRC test/STRFMT.cbl cbl/STRFMT?.cbl cbl/STRFMT??.cbl)
set(TEST_STRFMT_EXE tstrfmt)
set(TEST_STRFMT_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_STRFMT_EXE})
add_custom_command(
    OUTPUT ${TEST_STRFMT_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_STRFMT_PATH} ${TEST_STRFMT_SRC}
    DEPENDS ${TEST_STRFMT_SRC}
    COMMENT "Compiling test: ${TEST_STRFMT_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_STRFMT_EXE} ALL DEPENDS ${TEST_STRFMT_PATH})
add_test(NAME TestStrFormatting COMMAND ${TEST_STRFMT_PATH})

#
# Stellar age & population test.
#
set(TEST_STLRAGE_SRC test/STLRAGE.cbl cbl/STLRAGE.cbl ${RNG_DEPS})
set(TEST_STLRAGE_EXE tstlrage)
set(TEST_STLRAGE_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_STLRAGE_EXE})
add_custom_command(
    OUTPUT ${TEST_STLRAGE_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_STLRAGE_PATH} ${TEST_STLRAGE_SRC}
    DEPENDS ${TEST_STLRAGE_SRC}
    COMMENT "Compiling test: ${TEST_STLRAGE_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_STLRAGE_EXE} ALL DEPENDS ${TEST_STLRAGE_PATH})
add_test(NAME Test_stellar_age_and_population COMMAND ${TEST_STLRAGE_PATH})
