       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TEST-STELLAR-AGE.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 8, 2025.
       COPY          TESTENV.
      ******************************************************************
      *
      * Test how stellar age determining works, if it works.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STELLAR-AGE.
           05  BYR                     USAGE COMP-2.
           05  POPULATION              PIC XX.
               COPY STLRPOP.
       01  WS-BYR                      PIC 99V99 USAGE COMP-3.
       01  WS-LOOP                     USAGE COMP-1.

       PROCEDURE DIVISION.
      D    DISPLAY '[call GEN-SYSTEM-AGE]'
           CALL 'GEN-SYSTEM-AGE' USING WS-STELLAR-AGE
           COMPUTE WS-BYR ROUNDED = BYR
      D    DISPLAY ' ⇢ BYr 'WS-BYR' ('BYR')'
      D    DISPLAY ' ⇢ Pop 'POPULATION
           PERFORM VARYING WS-LOOP FROM 1 BY 1 UNTIL WS-LOOP > 100000
           EVALUATE TRUE
               WHEN POP-EX1 AND WS-BYR IS GREATER THAN 0.0
               WHEN POP-Y1 AND (WS-BYR < 0.1 OR > 1.85)
               WHEN POP-I1 AND (WS-BYR < 2.0 OR > 5.5)
               WHEN POP-O1 AND (WS-BYR < 5.6 OR > 9.1)
               WHEN POP-I2 AND (WS-BYR < 8.0 OR > 11.5)
               WHEN POP-EX2 AND WS-BYR < 10.0
                   GO TO AGE-FAILURE
           END-EVALUATE
           END-PERFORM.
      D    DISPLAY 'All OK (this time at least)!'
           STOP RUN.
      
       AGE-FAILURE.
           DISPLAY 'ERROR: age of 'WS-BYR' BYr ('BYR') '
                   'does not work with population 'POPULATION
           MOVE 112 TO RETURN-CODE
           STOP RUN.
