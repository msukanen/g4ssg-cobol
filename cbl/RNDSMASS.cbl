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

       LINKAGE SECTION.
       01  LK-STAR.
           COPY STARDATA.

       PROCEDURE DIVISION USING LK-STAR.
           CALL '3D6' USING D6
           EVALUATE TRUE
               WHEN D6 = 3
                   CALL '3D6' USING D6
                   EVALUATE TRUE
                       WHEN D6 <= 10 MOVE 2.0 TO MASS
                       WHEN OTHER MOVE 1.9 TO MASS
                   END-EVALUATE
               WHEN D6 = 4
                   CALL '3D6' USING D6
                   EVALUATE TRUE
                       WHEN D6 <= 8 MOVE 1.8 TO MASS
                       WHEN D6 <= 11 MOVE 1.7 TO MASS
                       WHEN OTHER MOVE 1.6 TO MASS
                   END-EVALUATE
               WHEN D6 = 5
                   CALL '3D6' USING D6
                   EVALUATE TRUE
                       WHEN D6 <= 7 MOVE 1.5 TO MASS
                       WHEN D6 <= 10 MOVE 1.45 TO MASS
                       WHEN D6 <= 12 MOVE 1.4 TO MASS
                       WHEN OTHER MOVE 1.35 TO MASS
                   END-EVALUATE
               WHEN D6 = 6
                   CALL '3D6' USING D6
                   EVALUATE TRUE
                       WHEN D6 <= 7 MOVE 1.3 TO MASS
                       WHEN D6 <= 9 MOVE 1.25 TO MASS
                       WHEN D6 = 10 MOVE 1.2 TO MASS
                       WHEN D6 <= 12 MOVE 1.15 TO MASS
                       WHEN OTHER MOVE 1.1 TO MASS
                   END-EVALUATE
               WHEN D6 = 7
                   CALL '3D6' USING D6
                   EVALUATE TRUE
                       WHEN D6 <= 7 MOVE 1.05 TO MASS
                       WHEN D6 <= 9 MOVE 1.0 TO MASS
                       WHEN D6 = 10 MOVE 0.95 TO MASS
                       WHEN D6 <= 12 MOVE 0.9 TO MASS
                       WHEN OTHER MOVE 0.85 TO MASS
                   END-EVALUATE
               WHEN D6 = 8
                   CALL '3D6' USING D6
                   EVALUATE TRUE
                       WHEN D6 <= 7 MOVE 0.8 TO MASS
                       WHEN D6 <= 9 MOVE 0.75 TO MASS
                       WHEN D6 = 10 MOVE 0.7 TO MASS
                       WHEN D6 <= 12 MOVE 0.65 TO MASS
                       WHEN OTHER MOVE 0.6 TO MASS
                   END-EVALUATE
               WHEN D6 = 9
                   CALL '3D6' USING D6
                   EVALUATE TRUE
                       WHEN D6 <= 8 MOVE 0.55 TO MASS
                       WHEN D6 <= 11 MOVE 0.5 TO MASS
                       WHEN OTHER MOVE 0.45 TO MASS
                   END-EVALUATE
               WHEN D6 = 10
                   CALL '3D6' USING D6
                   EVALUATE TRUE
                       WHEN D6 <= 8 MOVE 0.4 TO MASS
                       WHEN D6 <= 11 MOVE 0.35 TO MASS
                       WHEN OTHER MOVE 0.3 TO MASS
                   END-EVALUATE
               WHEN D6 = 11 MOVE 0.25 TO MASS
               WHEN D6 = 12 MOVE 0.2 TO MASS
               WHEN D6 = 13 MOVE 0.15 TO MASS
               WHEN OTHER MOVE 0.1 TO MASS
           END-EVALUATE

           IF MASS > MASSIVE-STAR-THRESHOLD THEN
                SET MASSIVE-STAR TO TRUE
           ELSE SET MASSIVE-STAR TO FALSE
           END-IF.
           GOBACK.
