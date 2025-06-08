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
       01  LK-EVO.
           COPY STLREVO.
       01  LK-STAR.
           COPY STARDATA.

       PROCEDURE DIVISION USING LK-EVO, LK-STAR.
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
                   IF LUMINOSITY-MAX = NOT-APPLICABLE THEN
                       MOVE LUMINOSITY-MIN TO LUMINOSITY
                   END-IF
           END-EVALUATE
           GOBACK.
