       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 DETERMINE-STELLAR-AGE.
       AUTHOR.                     Markku Sukanen.
      ******************************************************************
      *
      * Determine stellar age (and population).
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  D6                          PIC 999 USAGE COMP-3.
       01  D62                         PIC 999 USAGE COMP-3.
       01  WS-BASE-AGE                 PIC 99V99.
           COPY STLRPOP2.
      D01  WS-TMP-AGESTR               PIC Z9.99.

       LINKAGE SECTION.
       01  LK-STELLAR-POPULATION       PIC XX.
           COPY STLRPOP.
       01  LK-STELLAR-AGE              PIC 99V99.

       PROCEDURE DIVISION USING LK-STELLAR-POPULATION, LK-STELLAR-AGE.
      D    DISPLAY '[stellar-age]'.
           CALL '3D6' USING D6.
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 3
                   SET POP-E1 TO TRUE
                   SET BA-POP-E1 TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 6
                   SET POP-Y1 TO TRUE
                   SET BA-POP-Y1 TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 10
                   SET POP-I1 TO TRUE
                   SET BA-POP-I1 TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 14
                   SET POP-O1 TO TRUE
                   SET BA-POP-O1 TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 17
                   SET POP-I2 TO TRUE
                   SET BA-POP-I2 TO TRUE
               WHEN OTHER
                   SET POP-E2 TO TRUE
                   SET BA-POP-E2 TO TRUE
           END-EVALUATE.
      D    DISPLAY '  pop = 'LK-STELLAR-POPULATION.
           
           MOVE WS-BASE-AGE TO LK-STELLAR-AGE.
           CALL '1D6' USING D6.
           CALL '1D6' USING D62.
           IF NOT POP-E1 THEN
               EVALUATE TRUE
                   WHEN POP-Y1
                       COMPUTE LK-STELLAR-AGE
                             = LK-STELLAR-AGE
                             + (0.3 * (D6 - 1))
                             + (0.05 * (D62 - 1))
                   WHEN OTHER
                       COMPUTE LK-STELLAR-AGE
                             = LK-STELLAR-AGE
                             + (0.6 * (D6 - 1))
                             + (0.1 * (D62 - 1))
               END-EVALUATE
           END-IF.
      D    MOVE LK-STELLAR-AGE TO WS-TMP-AGESTR
      D    STRING FUNCTION TRIM(WS-TMP-AGESTR)
                   DELIMITED BY SIZE
                   INTO WS-TMP-AGESTR.
      D    MOVE FUNCTION TRIM(WS-TMP-AGESTR) TO WS-TMP-AGESTR
      D    DISPLAY '  byr = 'WS-TMP-AGESTR.
           GOBACK.
