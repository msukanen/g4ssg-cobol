       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GEN-SRCH-MASS.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 8, 2025.
      ******************************************************************
      *
      * Generate random mass-index.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY    RNG.
       COPY    CONST.
       01  WS-TMP-NUM0                 USAGE COMP-2.

       LINKAGE SECTION.
       01  LK-STAR.
           COPY STARDATA.

       PROCEDURE DIVISION USING LK-STAR.
           COPY 3D6.
           EVALUATE TRUE
               WHEN D6 = 3
                   COPY 3D6.
                   EVALUATE TRUE
                       WHEN D6 <= 10 MOVE 2.0 TO WS-TMP-NUM0
                       WHEN OTHER MOVE 1.9 TO WS-TMP-NUM0
                   END-EVALUATE
               WHEN D6 = 4
                   COPY 3D6.
                   EVALUATE TRUE
                       WHEN D6 <= 8 MOVE 1.8 TO WS-TMP-NUM0
                       WHEN D6 <= 11 MOVE 1.7 TO WS-TMP-NUM0
                       WHEN OTHER MOVE 1.6 TO WS-TMP-NUM0
                   END-EVALUATE
               WHEN D6 = 5
                   COPY 3D6.
                   EVALUATE TRUE
                       WHEN D6 <= 7 MOVE 1.5 TO WS-TMP-NUM0
                       WHEN D6 <= 10 MOVE 1.45 TO WS-TMP-NUM0
                       WHEN D6 <= 12 MOVE 1.4 TO WS-TMP-NUM0
                       WHEN OTHER MOVE 1.35 TO WS-TMP-NUM0
                   END-EVALUATE
               WHEN D6 = 6
                   COPY 3D6.
                   EVALUATE TRUE
                       WHEN D6 <= 7 MOVE 1.3 TO WS-TMP-NUM0
                       WHEN D6 <= 9 MOVE 1.25 TO WS-TMP-NUM0
                       WHEN D6 = 10 MOVE 1.2 TO WS-TMP-NUM0
                       WHEN D6 <= 12 MOVE 1.15 TO WS-TMP-NUM0
                       WHEN OTHER MOVE 1.1 TO WS-TMP-NUM0
                   END-EVALUATE
               WHEN D6 = 7
                   COPY 3D6.
                   EVALUATE TRUE
                       WHEN D6 <= 7 MOVE 1.05 TO WS-TMP-NUM0
                       WHEN D6 <= 9 MOVE 1.0 TO WS-TMP-NUM0
                       WHEN D6 = 10 MOVE 0.95 TO WS-TMP-NUM0
                       WHEN D6 <= 12 MOVE 0.9 TO WS-TMP-NUM0
                       WHEN OTHER MOVE 0.85 TO WS-TMP-NUM0
                   END-EVALUATE
               WHEN D6 = 8
                   COPY 3D6.
                   EVALUATE TRUE
                       WHEN D6 <= 7 MOVE 0.8 TO WS-TMP-NUM0
                       WHEN D6 <= 9 MOVE 0.75 TO WS-TMP-NUM0
                       WHEN D6 = 10 MOVE 0.7 TO WS-TMP-NUM0
                       WHEN D6 <= 12 MOVE 0.65 TO WS-TMP-NUM0
                       WHEN OTHER MOVE 0.6 TO WS-TMP-NUM0
                   END-EVALUATE
               WHEN D6 = 9
                   COPY 3D6.
                   EVALUATE TRUE
                       WHEN D6 <= 8 MOVE 0.55 TO WS-TMP-NUM0
                       WHEN D6 <= 11 MOVE 0.5 TO WS-TMP-NUM0
                       WHEN OTHER MOVE 0.45 TO WS-TMP-NUM0
                   END-EVALUATE
               WHEN D6 = 10
                   COPY 3D6.
                   EVALUATE TRUE
                       WHEN D6 <= 8 MOVE 0.4 TO WS-TMP-NUM0
                       WHEN D6 <= 11 MOVE 0.35 TO WS-TMP-NUM0
                       WHEN OTHER MOVE 0.3 TO WS-TMP-NUM0
                   END-EVALUATE
               WHEN D6 = 11 MOVE 0.25 TO WS-TMP-NUM0
               WHEN D6 = 12 MOVE 0.2 TO WS-TMP-NUM0
               WHEN D6 = 13 MOVE 0.15 TO WS-TMP-NUM0
               WHEN OTHER MOVE 0.1 TO WS-TMP-NUM0
           END-EVALUATE

           IF WS-TMP-NUM0 > MASSIVE-STAR-THRESHOLD THEN
                SET MASSIVE-STAR TO TRUE
           ELSE SET MASSIVE-STAR TO FALSE
           END-IF.

      *    Alter mass by ±0…MASS-VARIANCE units.
           CALL 'ALTER-VALUE-BY-UPTO'  USING MASS-VARIANCE,
                                       WS-TMP-NUM0, MASS.
           MOVE MASS TO INITIAL-MASS.
           GOBACK.
