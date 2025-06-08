       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GEN-SYSTEM-AGE.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 8, 2025
      ******************************************************************
      *
      * Generate random star system age and (typical) population of the
      * star(s) in it.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY        RNG.
       LINKAGE SECTION.
       01  LK-STLR-AGE.
           05  BYR                     USAGE COMP-2.
           05  POPULATION              PIC XX.
               COPY STLRPOP.

       PROCEDURE DIVISION USING LK-STLR-AGE.
           CALL '3D6' USING D6
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 3
                   SET POP-EX1 TO TRUE
      *            EX1 population stars are generally only a few million
      *            years old.
                   MOVE 0.0 TO BYR
               WHEN D6 IS LESS OR EQUAL TO 6 SET POP-Y1 TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 10 SET POP-I1 TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 14 SET POP-O1 TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 17 SET POP-I2 TO TRUE
               WHEN OTHER SET POP-EX2 TO TRUE
           END-EVALUATE

           IF NOT POP-EX1 THEN
               CALL '1D6' USING D6
               CALL '1D6' USING D62
               IF POP-Y1 THEN
                   COMPUTE BYR = 0.1
                           + (0.3 * (D6 - 1))
                           + (0.05 * (D62 - 1))
               ELSE
                   COMPUTE BYR = (0.6 * (D6 - 1)) + (0.1 * (D62 - 1))
                   EVALUATE TRUE
                       WHEN POP-I1 COMPUTE BYR = 2.0 + BYR
                       WHEN POP-O1 COMPUTE BYR = 5.6 + BYR
                       WHEN POP-I2 COMPUTE BYR = 8.0 + BYR
                       WHEN POP-EX2 COMPUTE BYR = 10.0 + BYR
                   END-EVALUATE
               END-IF
           END-IF
           GOBACK.
