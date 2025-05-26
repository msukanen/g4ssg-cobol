cmake_minimum_required(VERSION 3.10)
project(G4SSG)

set(COBOL_SOURCE WRAPPER.cob
    MAIN.cob
    sub/rng/*.cob
    sub/ORBIT*.cob
)
set(TEST1_SOURCE test/VARIATST.cob
    sub/rng/ALTERVA?.cob
)
set(TEST2_SOURCE test/ORBITST1.cob
    sub/ORBITDET.cob
    sub/rng/?D6.cob
)
set(TEST3_SOURCE test/ORBITST2.cob
    sub/ORBITLIM.cob
    sub/rng/?D6.cob
)

set(COBOL_COMPILER cobc)
set(COBOL_FLAGS -x)
set(COBOL_DEBUG )
set(OUT_EXE bin/g4ssg)
set(TEST1_EXE bin/testvar)
set(TEST2_EXE bin/testorbd)
set(TEST3_EXE bin/testorbl)

add_custom_command(
    OUTPUT ${OUT_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${OUT_EXE} ${COBOL_SOURCE}
    DEPENDS ${COBOL_SOURCE}
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
    COMMENT "Compiling test 1 ..."
)

add_custom_target(
    build_cobol_test1 ALL
    DEPENDS ${TEST1_EXE}
)

add_custom_command(
    OUTPUT ${TEST2_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST2_EXE} ${TEST2_SOURCE}
    DEPENDS ${TEST2_SOURCE}
    COMMENT "Compiling test 2 ..."
)

add_custom_target(
    build_cobol_test2 ALL
    DEPENDS ${TEST2_EXE}
)

add_custom_command(
    OUTPUT ${TEST3_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} ${COBOL_DEBUG} -o ${TEST3_EXE} ${TEST3_SOURCE}
    DEPENDS ${TEST3_SOURCE}
    COMMENT "Compiling test 3 ..."
)

add_custom_target(
    build_cobol_test3 ALL
    DEPENDS ${TEST3_EXE}
)
