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
           GOBACK.
