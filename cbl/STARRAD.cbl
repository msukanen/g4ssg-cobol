       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-RADIUS.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 9, 2025.
      ******************************************************************
      *
      * Determine a star's radius based on its luminosity and
      * temperature (in Kelvin).
      *
      ******************************************************************
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-STAR.
           COPY STARDATA.

       PROCEDURE DIVISION USING LK-STAR.
           IF WHITE-DWARF OR NEUTRON-STAR OR BLACK-HOLE THEN
                MOVE 0.0 TO RADIUS
           ELSE
               COMPUTE RADIUS
                     = (FUNCTION SQRT(CURRENT-LUM) * 155000)
                     / (TEMPERATURE * TEMPERATURE)
           END-IF.
           GOBACK.
