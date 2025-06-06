cmake_minimum_required(VERSION 3.10)
project(G4SSG)

enable_testing()

set(CMAKE_CURRENT_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}/bin")

set(CPY_DIR cbl/cpy)
set(COBOL_COMPILER cobc)
set(COBOL_FLAGS -x -I${CPY_DIR})
set(COBOL_DEBUG )

set(CPY_DEPS ${CPY_DIR}/*.cpy)
set(STAR_CPY_DEPS ${CPY_DIR}/STAR*.cpy ${CPY_DIR}/ORBZONE.cpy ${CPY_DIR}/GGA.cpy)
set(ELEM_CPY_DEPS ${CPY_DIR}/ETYPE.cpy)

#
# Main App.
#
set(MAIN_SOURCE WRAPPER.cbl
    cbl/MAIN.cbl
    cbl/rng/*.cbl
    cbl/D*.cbl
    cbl/G*.cbl
    cbl/O*.cbl
    cbl/P*.cbl
    cbl/S*.cbl
    cbl/X*.cbl
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
# DLUM testing.
#
set(TEST_DLUM_SRC test/DLUM.cbl cbl/DLUM.cbl cbl/rng/ALTERVA?.cbl)
set(TEST_DLUM_EXE tdlum)
set(TEST_DLUM_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_DLUM_EXE})
add_custom_command(
    OUTPUT ${TEST_DLUM_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_DLUM_PATH} ${TEST_DLUM_SRC}
    DEPENDS ${TEST_DLUM_SRC} cbl/cpy/STARSTG.cpy cbl/cpy/STLREVO.cpy
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
    DEPENDS ${TEST_DSEQ_SRC}  cbl/cpy/STARSTG.cpy cbl/cpy/STLREVO.cpy
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
    DEPENDS ${TEST_OECC_SRC} cbl/cpy/ORBSEP.cpy
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
    DEPENDS ${TEST_OSEP_SRC} cbl/cpy/ORBSEP.cpy cbl/cpy/ORBSEPV.cpy
    COMMENT "Compiling test: ${TEST_OSEP_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_OSEP_EXE} ALL DEPENDS ${TEST_OSEP_PATH})
add_test(NAME TestOSep COMMAND ${TEST_OSEP_PATH})

#
# ORBZ testing.
#
set(TEST_ORBZ_SRC test/ORBZ.cbl cbl/ORBZ.cbl cbl/STRFMT5.cbl)
set(TEST_ORBZ_EXE torbz)
set(TEST_ORBZ_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_ORBZ_EXE})
add_custom_command(
    OUTPUT ${TEST_ORBZ_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_ORBZ_PATH} ${TEST_ORBZ_SRC}
    DEPENDS ${TEST_ORBZ_SRC} ${STAR_CPY_DEPS}
    COMMENT "Compiling test: ${TEST_ORBZ_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_ORBZ_EXE} ALL DEPENDS ${TEST_ORBZ_PATH})
add_test(NAME TestOZone COMMAND ${TEST_ORBZ_PATH})

#
# PGGA testing.
#
set(TEST_PGGA_SRC test/PGGA.cbl cbl/PGGA.cbl cbl/XPGGA.cbl cbl/rng/?D6.cbl)
set(TEST_PGGA_EXE tpgga)
set(TEST_PGGA_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_PGGA_EXE})
add_custom_command(
    OUTPUT ${TEST_PGGA_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_PGGA_PATH} ${TEST_PGGA_SRC}
    DEPENDS ${TEST_PGGA_SRC} ${STAR_CPY_DEPS}
    COMMENT "Compiling test: ${TEST_PGGA_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_PGGA_EXE} ALL DEPENDS ${TEST_PGGA_PATH})
add_test(NAME TestPGGA COMMAND ${TEST_PGGA_PATH})

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
# OSPC testing.
#
set(TEST_OSPC_SRC test/OSPC.cbl cbl/OSPC.cbl cbl/STRFMT5.cbl cbl/rng/?D6.cbl cbl/rng/ALTERVA1.cbl)
set(TEST_OSPC_EXE tospc)
set(TEST_OSPC_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_OSPC_EXE})
add_custom_command(
    OUTPUT ${TEST_OSPC_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_OSPC_PATH} ${TEST_OSPC_SRC}
    DEPENDS ${TEST_OSPC_SRC} cbl/cpy/ORBDIR.cpy cbl/cpy/ORBAVAIL.cpy
    COMMENT "Compiling test: ${TEST_OSPC_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_OSPC_EXE} ALL DEPENDS ${TEST_OSPC_PATH})
add_test(NAME TestOSpacing COMMAND ${TEST_OSPC_PATH})

#
# GELEMENT testing.
#
set(TEST_GELEMENT_SRC test/GELEMENT.cbl cbl/GASTBELT.cbl cbl/GGASGNT.cbl cbl/GTERREST.cbl cbl/rng/?D6.cbl)
set(TEST_GELEMENT_EXE tgelemt)
set(TEST_GELEMENT_PATH ${CMAKE_CURRENT_BINARY_DIR}/${TEST_GELEMENT_EXE})
add_custom_command(
    OUTPUT ${TEST_GELEMENT_PATH}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST_GELEMENT_PATH} ${TEST_GELEMENT_SRC}
    DEPENDS ${TEST_GELEMENT_SRC} ${ELEM_CPY_DEPS} cbl/cpy/EASTBELT.cpy cbl/cpy/EGG.cpy cbl/cpy/ETERR.cpy
    COMMENT "Compiling test: ${TEST_GELEMENT_EXE}"
    VERBATIM
)
add_custom_target(build_${TEST_GELEMENT_EXE} ALL DEPENDS ${TEST_GELEMENT_PATH})
add_test(NAME TestGenAstBelt COMMAND ${TEST_GELEMENT_PATH})
