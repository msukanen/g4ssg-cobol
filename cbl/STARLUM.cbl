       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-LUMINOSITY.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 8, 2025.
      ******************************************************************p.104
      *
      * Determine a star's current luminosity (compared to Sol).
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY    RNG.
       COPY    CONST.
       01  WS-NUM                      USAGE COMP-2.

       LINKAGE SECTION.
       01  LK-AGE.
           COPY STLRAGE.
       01  LK-EVO.
           COPY STLREVO.
       01  LK-STAR.
           COPY STARDATA.

       PROCEDURE DIVISION USING LK-AGE, LK-EVO, LK-STAR.
           EVALUATE TRUE
               WHEN WHITE-DWARF OR NEUTRON-STAR
      *            WD & N have about negligible luminosity, rarely higher
      *            than 0.001 sol.
                   COMPUTE WS-NUM = FUNCTION RANDOM * 0.0099 + 0.0001
                   MOVE WS-NUM TO LUMINOSITY
               WHEN BLACK-HOLE
      *            Black holes by default have no luminosity at all …
                   MOVE 0.0 TO LUMINOSITY
               WHEN CLASS-V OR CLASS-VI
                   IF LUMINOSITY-MAX = NOT-APPLICABLE
                      OR MASSIVE-STAR THEN
                       MOVE LUMINOSITY-MIN TO LUMINOSITY
                   ELSE
                       COMPUTE LUMINOSITY =
                               LUMINOSITY-MIN + (
                                (BYR / SPAN-M) *
                                (LUMINOSITY-MAX - LUMINOSITY-MIN)
                                )
                   END-IF
               WHEN CLASS-IV
                   IF LUMINOSITY-MAX = NOT-APPLICABLE
                      OR MASSIVE-STAR THEN
                       MOVE LUMINOSITY-MIN TO LUMINOSITY
                   ELSE
                       MOVE LUMINOSITY-MAX TO LUMINOSITY
                   END-IF
               WHEN OTHER
                   IF LUMINOSITY-MAX = NOT-APPLICABLE
                      OR MASSIVE-STAR THEN
                       COMPUTE LUMINOSITY = 25.0 * LUMINOSITY-MIN
                   ELSE
                       COMPUTE LUMINOSITY = 25.0 * LUMINOSITY-MAX
                   END-IF
           END-EVALUATE

           MOVE LUMINOSITY TO WS-NUM
           CALL 'ALTER-VALUE-BY-PERCENTAGE' USING
                   TEN-PERCENT, WS-NUM, LUMINOSITY
           GOBACK.
