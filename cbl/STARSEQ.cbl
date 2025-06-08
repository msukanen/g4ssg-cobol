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
       WORKING-STORAGE SECTION.
       COPY    CONST.
       01  WS-MS                       USAGE COMP-2.
       01  WS-MSG                      USAGE COMP-2.
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
           IF MASS OF LK-STAR > MASSIVE-STAR-THRESHOLD THEN
                PERFORM DET-M-STAGE
           ELSE PERFORM DET-N-STAGE END-IF.
           GOBACK.

       DET-N-STAGE.
           IF SPAN-M = NOT-APPLICABLE THEN SET CLASS-V TO TRUE
           ELSE
               COMPUTE WS-MS = SPAN-M + SPAN-S
               COMPUTE WS-MSG = SPAN-M + SPAN-S + SPAN-G
               EVALUATE TRUE
               WHEN (SPAN-S = NOT-APPLICABLE AND BYR > SPAN-M)
                 OR BYR > WS-MSG
                   SET WHITE-DWARF TO TRUE
               WHEN BYR > WS-MS
                   SET CLASS-III TO TRUE
               WHEN BYR > SPAN-M
                   SET CLASS-IV TO TRUE
               WHEN OTHER
                   SET CLASS-V TO TRUE
           END-EVALUATE END-IF.
           EXIT PARAGRAPH.

       DET-M-STAGE.
           IF SPAN-S <= LK-SYSTEM-AGE THEN SET CLASS-V TO TRUE
           ELSE EVALUATE TRUE
               WHEN MASS OF LK-STAR <= 8.0
                   SET WHITE-DWARF TO TRUE
               WHEN MASS OF LK-STAR <= 25.0
                   SET NEUTRON-STAR TO TRUE
               WHEN OTHER
                   SET BLACK-HOLE TO TRUE
           END-EVALUATE END-IF.
           EXIT PARAGRAPH.
