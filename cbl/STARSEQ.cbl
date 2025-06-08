       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-LIFE-STAGE.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 8, 2025.
      ******************************************************************
      *
      * Determine a star's life sequence based on its age and mass-
      * index.
      *
      ******************************************************************
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-SYSTEM-AGE.
           05  BYR                     USAGE COMP-2.
           05  POPULATION              PIC XX.
               COPY STLRPOP.
       01  LK-EVO.
           COPY STLREVO.
       01  LK-STAR.
           COPY STARDATA.

       PROCEDURE DIVISION USING LK-SYSTEM-AGE, LK-EVO, LK-STAR.
           GOBACK.
