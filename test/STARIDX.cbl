       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TEST_GET-MASS-INDEX.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 8, 2025.
       COPY          TESTENV.
      ******************************************************************
      *
      * Testing random mass-index generation.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SRCH-MASS                USAGE COMP-2.
       01  WS-STELLAR-EVO.
           05  EVO-COUNT               PIC 999 USAGE COMP-5 VALUE 34.
           05  EVO                     OCCURS 34 TIMES
                                       INDEXED BY EVO-IDX.
               COPY STLREVO.
       01  WS-INDEX                    INDEX.

       PROCEDURE DIVISION.
           SET EVO-IDX    TO 1. MOVE 2.0 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.9 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.8 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.7 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.6 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.5 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.45 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.4 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.35 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.3 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.25 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.2 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.15 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.1 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.05 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 1.0 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.95 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.9 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.85 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.8 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.75 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.7 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.65 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.6 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.55 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.5 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.45 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.4 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.35 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.3 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.25 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.2 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.15 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.1 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.1 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.1 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.1 TO MASS(EVO-IDX)
           SET EVO-IDX UP BY 1. MOVE 0.1 TO MASS(EVO-IDX)
           PERFORM VARYING WS-SRCH-MASS
                   FROM 0.001 BY 0.04
                   UNTIL WS-SRCH-MASS > 3.0
               CALL 'GET-MASS-INDEX'   USING WS-SRCH-MASS,
                                       WS-STELLAR-EVO, WS-INDEX
           END-PERFORM
           STOP RUN.
           