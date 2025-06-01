       IDENTIFICATION DIVISION.
       PROGRAM-ID. DETERMINE-ORBITAL-SEPARATION.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Determine orbital separation (in AU).
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY RNG.
       01  WS-MUL                      PIC 999V99 USAGE COMP-3.
       01  WS-AV0                      USAGE COMP-2.
       01  WS-AV1                      USAGE COMP-2.
       01  WS-AV2                      USAGE COMP-2.

       LINKAGE SECTION.
      * 3rd in trinary generally is much further away than the 2nd star.
       01  LK-3RD-IN-TRINARY           PIC X.                            Y / N
       01  LK-SEPARATION.
           COPY ORBSEPV.

       PROCEDURE DIVISION USING LK-3RD-IN-TRINARY, LK-SEPARATION.
           CALL '3D6' USING D6
           IF LK-3RD-IN-TRINARY = 'Y' THEN COMPUTE D6 = D6 + 6.
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 6
                   SET SEP-VERY-CLOSE TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 9
                   SET SEP-CLOSE TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 11
                   SET SEP-MODERATE TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 14
                   SET SEP-WIDE TO TRUE
               WHEN OTHER
                   SET SEP-DISTANT TO TRUE
           END-EVALUATE

           CALL 'DETERMINE-ORBITAL-ECCENTRICITY' USING
                                       SEP-CATEGORY, SEP-ECC
           EVALUATE TRUE
               WHEN SEP-VERY-CLOSE
                   MOVE 0.05 TO WS-MUL
                   MOVE 0.025 TO WS-AV0
               WHEN SEP-CLOSE
                   MOVE 0.5 TO WS-MUL
                   MOVE 0.25 TO WS-AV0
               WHEN SEP-MODERATE
                   MOVE 2.0 TO WS-MUL
                   MOVE 1.0 TO WS-AV0
               WHEN SEP-WIDE
                   MOVE 10.0 TO WS-MUL
                   MOVE  5.0 TO WS-AV0
               WHEN OTHER
                   MOVE 50.0 TO WS-MUL
                   MOVE 25.0 TO WS-AV0
           END-EVALUATE

           CALL '2D6' USING D6
           COMPUTE WS-AV1 = WS-MUL * D6

      * Alter AU by up to half of initial multiplier.
           CALL 'ALTER-VALUE-BY-UPTO' USING WS-AV0, WS-AV1, WS-AV2
           COMPUTE SEP-MIN ROUNDED = (1.0 - SEP-ECC) * WS-AV2
           COMPUTE SEP-MAX ROUNDED = (1.0 + SEP-ECC) * WS-AV2
           COMPUTE SEP-AVG ROUNDED = WS-AV2
           GOBACK.
