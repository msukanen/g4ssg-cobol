       IDENTIFICATION DIVISION.
       PROGRAM-ID. DETERMINE-SEQUENCE.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Determine a star's life sequence based on its mass-index and
      * age (in billions of years, naturally).
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CONST.

       LINKAGE SECTION.
       01  LK-AGE                      PIC 99V99.
       01  LK-EVO.
           COPY STLREVO.
       01  LK-STAGE                    PIC X(4).
           COPY STARSTG.

       PROCEDURE DIVISION USING LK-EVO, LK-STAGE.
           IF EVO-L-MAX IS EQUAL TO INF-LIFESPAN
               SET CLASS-V TO TRUE
           END-IF
           GOBACK.
