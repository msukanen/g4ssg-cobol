       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-GG-ARRANGEMENT.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 3, 2025
      ****************************************************************** p.105-
      *                                                                    106
      * Determine gas giant arrangement if any such giant happens to be
      * in the star system — about half the time there won't be any.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY RNG.
       01  WS-RNG                      USAGE COMP-2.

       LINKAGE SECTION.
       01  LK-STAR.
           COPY STAR.

       PROCEDURE DIVISION USING LK-STAR.
           DISPLAY 'mass ' MASS
           CALL '3D6' USING D6
           EVALUATE TRUE                                                p.106
               WHEN D6 IS LESS OR EQUAL TO 10
                   SET NO-GAS-GIANT TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 12
                   SET CONVENTIONAL-GAS-GIANT TO TRUE
                   DISPLAY 'OZ-SL 'OZ-SNOW-LINE
                   COMPUTE WS-RNG =
                           ((FUNCTION RANDOM / 2) + 1) * OZ-SNOW-LINE
               WHEN D6 IS LESS OR EQUAL TO 14
                   SET ECCENTRIC-GAS-GIANT TO TRUE
                   CALL '1D6' USING D6
                   COMPUTE WS-RNG = 0.125 * D6 * OZ-SNOW-LINE
               WHEN OTHER
                   SET EPISTELLAR-GAS-GIANT TO TRUE
                   CALL '3D6' USING D6
                   COMPUTE WS-RNG = D6 / 10 * OZ-INNER-LIMIT
           END-EVALUATE
           IF NOT NO-GAS-GIANT THEN COMPUTE GG-DISTANCE = WS-RNG.
           GOBACK.
