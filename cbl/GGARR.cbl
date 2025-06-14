       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GEN-GAS-GIANT-ARRANGEMENT.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 14, 2025.
      ******************************************************************
      *
      * Generate random, initial, gas giant arrangement for a star
      * system.  If no such giant exist, mark it as not present.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY    RNG.
       01  WS-TMP-NUM0                 USAGE COMP-2.
       01  WS-TMP-NUM1                 USAGE COMP-2.
       LINKAGE SECTION.
       01  LK-LIMITS.
           COPY ORBLIM.
       01  LK-GGA.
           05  ARRANGEMENT             PIC X VALUE '-'.
               COPY GGARR.cpy.
           05  DISTANCE                USAGE COMP-2.

       PROCEDURE DIVISION USING LK-LIMITS, LK-GGA.
           COPY 3D6.
           EVALUATE TRUE
               WHEN D6 <= 10
                   SET NO-GAS-GIANT TO TRUE                             No GG
                   GOBACK

      *        Avg. distance to the major GG.  We assume that such
      *        object's orbit is not too noticeably eccentric lest it'll
      *        be too unstable in the long term.
               WHEN D6 <= 666
      *        WHEN D6 <= 12
                   SET CONVENTIONAL-GG TO TRUE
      D            DISPLAY 'SET TO CONVENTIONAL-GG'
                   COPY 2D6.
                   DISPLAY '2D6 ⇢ 'D6
                   DISPLAY 'SNOW-LINE ⇢ 'SNOW-LINE
                   COMPUTE DISTANCE = 0.05 * (D6 - 2) + 1 * SNOW-LINE
               WHEN D6 <= 14
                   SET ECCENTRIC-GG TO TRUE
      D            DISPLAY 'SET TO ECCENTRIC-GG'
                   COPY 1D6.
                   COMPUTE DISTANCE = 0.125 * D6 * SNOW-LINE
               WHEN OTHER
                   SET EPISTELLAR-GG TO TRUE
      D            DISPLAY 'SET TO EPISTELLAR-GG'
                   COPY 3D6.
                   COMPUTE DISTANCE = 0.1 * D6 * INNER-LIMIT
           END-EVALUATE.
           GOBACK.
