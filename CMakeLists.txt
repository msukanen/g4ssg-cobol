cmake_minimum_required(VERSION 3.10)
project(G4SSG)

set(COBOL_SOURCE WRAPPER.cob MAIN.cob DICER*.cob DICEN*.cob ?D6.cob)

set(COBOL_COMPILER cobc)
set(COBOL_FLAGS -x)
set(OUT_EXE g4ssg)

add_custom_command(
    OUTPUT ${OUT_EXE}
    COMMAND ${COBOL_COMPILER} ${COBOL_FLAGS} -o ${OUT_EXE} ${COBOL_SOURCE}
    DEPENDS ${COBOL_SOURCE}
    COMMENT "Compiling G4SSG ..."
)

add_custom_target(
    build_cobol ALL
    DEPENDS ${OUT_EXE}
)
