       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-DETERMINE-SEQUENCE.
       AUTHOR.     Markku Sukanen.
       COPY TESTENV.
      ******************************************************************
      *
      * Test star's life sequence determining.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AGE                      PIC 99V99.
       01  WS-EVO.
           COPY STLREVO.
       01  WS-STAGE                    PIC X(4).
           COPY STARSTG.
       
       PROCEDURE DIVISION.
           MOVE 3.0 TO WS-AGE.
           CALL 'DETERMINE-SEQUENCE' USING WS-AGE, WS-EVO, WS-STAGE.
           STOP RUN.
