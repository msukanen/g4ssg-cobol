       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ORBITAL-ECCENTRICITY.
       AUTHOR.     Markku Sukanen.
       COPY TESTENV.
      ******************************************************************
      *
      * Test orbital eccentricity.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SEPARATION                  PIC 9.
           COPY ORBSEP.
       01  ECCENTRICITY                PIC 9V99 USAGE COMP-3.

       PROCEDURE DIVISION.
           SET SEP-MODERATE TO TRUE
           CALL 'DETERMINE-ORBITAL-ECCENTRICITY'
                                       USING SEPARATION, ECCENTRICITY.
           STOP RUN.
