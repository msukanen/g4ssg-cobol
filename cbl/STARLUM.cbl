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
                   MOVE WS-NUM TO CURRENT-LUM
                   MOVE LUMINOSITY-MIN TO INITIAL-LUM
               WHEN BLACK-HOLE
      *            Black holes by default have no luminosity at all …
                   MOVE 0.0 TO LUMINOSITY
                   MOVE LUMINOSITY-MIN TO INITIAL-LUM
               WHEN CLASS-V OR CLASS-VI
                   IF LUMINOSITY-MAX = NOT-APPLICABLE
                      OR MASSIVE-STAR THEN
                       MOVE LUMINOSITY-MIN TO CURRENT-LUM
                   ELSE
                       COMPUTE CURRENT-LUM =
                               LUMINOSITY-MIN + (
                                (BYR / SPAN-M) *
                                (LUMINOSITY-MAX - LUMINOSITY-MIN)
                                )
                   END-IF
      *            Naturally, current and initial lumiosity are one and
      *            the same - no change has yet happened.
                   MOVE CURRENT-LUM TO INITIAL-LUM
               WHEN CLASS-IV
                   IF LUMINOSITY-MAX = NOT-APPLICABLE
                      OR MASSIVE-STAR THEN
                       MOVE LUMINOSITY-MIN TO CURRENT-LUM
                   ELSE
                       MOVE LUMINOSITY-MAX TO CURRENT-LUM
                   END-IF
                   MOVE LUMINOSITY-MIN TO INITIAL-LUM
               WHEN OTHER
                   IF LUMINOSITY-MAX = NOT-APPLICABLE
                      OR MASSIVE-STAR THEN
                       COMPUTE CURRENT-LUM = 25.0 * LUMINOSITY-MIN
                   ELSE
                       COMPUTE CURRENT-LUM = 25.0 * LUMINOSITY-MAX
                   END-IF
                   MOVE LUMINOSITY-MIN TO INITIAL-LUM
           END-EVALUATE

           MOVE LUMINOSITY TO WS-NUM
           CALL 'ALTER-VALUE-BY-PERCENTAGE' USING
                   TEN-PERCENT, WS-NUM, LUMINOSITY
           GOBACK.
