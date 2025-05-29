cmake_minimum_required(VERSION 3.10)
project(G4SSG)

set(MAIN_SOURCE WRAPPER.cbl
    cbl/MAIN.cbl
    cbl/rng/*.cbl
    cbl/ORBIT*.cbl cbl/PLGG.cbl
)
set(TEST1_SOURCE test/VARIATST.cbl cbl/rng/ALTERVA?.cbl)
set(TEST2_SOURCE test/ORBITDET.cbl cbl/ORBITDET.cbl cbl/rng/?D6.cbl)
set(TEST3_SOURCE test/ORBITLIM.cbl cbl/ORBITLIM.cbl cbl/rng/?D6.cbl)
set(TEST4_SOURCE test/ORBITSPC.cbl cbl/ORBITSPC.cbl cbl/rng/ALTERVA1.cbl cbl/rng/?D6.cbl)

set(COBOL_COMPILER cobc)
set(COBOL_FLAGS -x -Icbl/cpy)
set(COBOL_DEBUG )
set(OUT_EXE bin/g4ssg)

set(TEST1_EXE bin/testvar)
set(TEST2_EXE bin/testorbd)
set(TEST3_EXE bin/testorbl)
set(TEST4_EXE bin/testorbs)

add_custom_command(
    OUTPUT ${OUT_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${OUT_EXE} ${MAIN_SOURCE}
    DEPENDS ${MAIN_SOURCE}
    COMMENT "Compiling G4SSG ..."
)

add_custom_target(
    build_cobol ALL
    DEPENDS ${OUT_EXE}
)

add_custom_command(
    OUTPUT ${TEST1_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST1_EXE} ${TEST1_SOURCE}
    DEPENDS ${TEST1_SOURCE}
    COMMENT "Compiling test ${TEST1_EXE} ..."
)

add_custom_target(
    build_cobol_test1 ALL
    DEPENDS ${TEST1_EXE}
)

add_custom_command(
    OUTPUT ${TEST2_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST2_EXE} ${TEST2_SOURCE}
    DEPENDS ${TEST2_SOURCE}
    COMMENT "Compiling test ${TEST2_EXE} ..."
)

add_custom_target(
    build_cobol_test2 ALL
    DEPENDS ${TEST2_EXE}
)

add_custom_command(
    OUTPUT ${TEST3_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST3_EXE} ${TEST3_SOURCE}
    DEPENDS ${TEST3_SOURCE}
    COMMENT "Compiling test ${TEST3_EXE} ..."
)

add_custom_target(
    build_cobol_test3 ALL
    DEPENDS ${TEST3_EXE}
)

add_custom_command(
    OUTPUT ${TEST4_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST4_EXE} ${TEST4_SOURCE}
    DEPENDS ${TEST4_SOURCE}
    COMMENT "Compiling test ${TEST4_EXE} ..."
)

add_custom_target(
    build_cobol_test4 ALL
    DEPENDS ${TEST4_EXE}
)
