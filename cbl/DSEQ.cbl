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
       01  WS-TMP1                     PIC 9(5)V9(5) USAGE COMP-3.

       LINKAGE SECTION.
       01  LK-AGE                      PIC 99V99.
       01  LK-EVO.
           COPY STLREVO.
       01  LK-STAGE                    PIC X(4).
           COPY STARSTG.

       PROCEDURE DIVISION USING LK-EVO, LK-AGE, LK-STAGE.
      D    DISPLAY '[determine-sequence]' NO ADVANCING
           EVALUATE TRUE
               WHEN EVO-M-SPAN IS EQUAL TO INF-LIFESPAN
                    OR LK-AGE IS LESS OR EQUAL TO EVO-M-SPAN
                   SET CLASS-V TO TRUE
               WHEN EVO-S-SPAN IS EQUAL TO NOT-AVAILABLE
                    AND LK-AGE > EVO-M-SPAN
                   SET CLASS-D TO TRUE
               WHEN LK-AGE > (EVO-M-SPAN + EVO-S-SPAN + EVO-G-SPAN)
                   SET CLASS-D TO TRUE
               WHEN LK-AGE > EVO-M-SPAN + EVO-S-SPAN
                   SET CLASS-III TO TRUE
               WHEN LK-AGE > EVO-M-SPAN
                   SET CLASS-IV TO TRUE
               WHEN OTHER
                   SET CLASS-X TO TRUE
           END-EVALUATE
      D    DISPLAY ' seq = 'LK-STAGE
           GOBACK.
