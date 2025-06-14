cmake_minimum_required(VERSION 3.19)
project(G4SSG)

enable_testing()

set(CMAKE_BUILD_PARALLEL_LEVEL 8)
set(CMAKE_CURRENT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/bin")

set(CPY_DIR cpy)
set(CPY_DEPS ${CPY_DIR}/*.cpy)
set(TEST_DEPS ${CPY_DIR}/TESTENV.cpy)
set(RNG_DEPS cbl/rng/ALTERVA?.cbl)

set(COBOL_COMPILER cobc)
set(COBOL_FLAGS -x -I${CPY_DIR})
set(COBOL_DEBUG )

#
# Main App.
#
set(MAIN_SOURCE WRAPPER.cbl
    cbl/MAIN.cbl
    ${RNG_DEPS}
    cbl/GG*.cbl
    cbl/ORB*.cbl
    cbl/RNDSMASS.cbl
    cbl/STRFMT.cbl
    cbl/STLR*.cbl
    cbl/STAR*.cbl
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
add_custom_target(
    run
    COMMAND ${MAIN_PATH}
    DEPENDS build_cobol
    COMMENT "GENERATING A NEW STAR SYSTEM. Hold on to your hats! Here we go …"
)

#
# Some foobar string testing.
#
set(TEST_STRFMT_SRC test/STRFMT.cbl cbl/STRFMT.cbl)
set(TEST_STRFMT_EXE tstrfmt)
set(TEST_STRFMT_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_STRFMT_EXE})
add_custom_command(
    OUTPUT ${TEST_STRFMT_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_STRFMT_PATH} ${TEST_STRFMT_SRC}
    DEPENDS ${TEST_STRFMT_SRC} ${TEST_DEPS}
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
    DEPENDS ${TEST_STLRAGE_SRC} ${TEST_DEPS}
    COMMENT "Compiling test: ${TEST_STLRAGE_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_STLRAGE_EXE} ALL DEPENDS ${TEST_STLRAGE_PATH})
add_test(NAME Test_stellar_age_and_population COMMAND ${TEST_STLRAGE_PATH})

#
# Stellar mass index and such …
#
set(TEST_STARIDX_SRC test/STARIDX.cbl cbl/STARIDX.cbl cbl/RNDSMASS.cbl ${RNG_DEPS})
set(TEST_STARIDX_EXE tstaridx)
set(TEST_STARIDX_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_STARIDX_EXE})
add_custom_command(
    OUTPUT ${TEST_STARIDX_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_STARIDX_PATH} ${TEST_STARIDX_SRC}
    DEPENDS ${TEST_STARIDX_SRC} ${TEST_DEPS}
    COMMENT "Compiling test: ${TEST_STARIDX_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_STARIDX_EXE} ALL DEPENDS ${TEST_STARIDX_PATH})
add_test(NAME Test_stellar_mass_index COMMAND ${TEST_STARIDX_PATH})

#
# Orbital separation …
#
set(TEST_ORBSEP_SRC test/ORBSEP.cbl cbl/ORBSEP.cbl ${RNG_DEPS})
set(TEST_ORBSEP_EXE torbsep)
set(TEST_ORBSEP_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_ORBSEP_EXE})
add_custom_command(
    OUTPUT ${TEST_ORBSEP_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_ORBSEP_PATH} ${TEST_ORBSEP_SRC}
    DEPENDS ${TEST_ORBSEP_SRC} ${TEST_DEPS} ${CPY_DIR}/SEPCATEG.cpy
    COMMENT "Compiling test: ${TEST_ORBSEP_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_ORBSEP_EXE} ALL DEPENDS ${TEST_ORBSEP_PATH})
add_test(NAME "TEST Orbital Separation" COMMAND ${TEST_ORBSEP_PATH})
