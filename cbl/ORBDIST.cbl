       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GENERATE-ORBIT-DISTANCE.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 12, 2025.
      ******************************************************************
      *
      * Generate more or less random orbit distance.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TMP-NUM0                 USAGE COMP-2.
       01  WS-TMP-NUM1                 USAGE COMP-2.
       01  WS-TMP-NUM2                 USAGE COMP-2.
       
       LINKAGE SECTION.
       01  LK-SEP-CATEGORY             PIC XX.
           COPY SEPCATEG.
       01  LK-AVG-DISTANCE             USAGE COMP-2.

       PROCEDURE DIVISION USING LK-SEP-CATEGORY, LK-AVG-DISTANCE.
           EVALUATE TRUE
               WHEN SEP-V-CLOSE
                   MOVE 0.05 TO WS-TMP-NUM0
               WHEN SEP-CLOSE
                   MOVE 0.5 TO WS-TMP-NUM0
               WHEN SEP-MODERATE
                   MOVE 2.0 TO WS-TMP-NUM0
               WHEN SEP-WIDE
                   MOVE 10.0 TO WS-TMP-NUM0
               WHEN OTHER
                   MOVE 50.0 TO WS-TMP-NUM0
           END-EVALUATE.
           
           COMPUTE WS-TMP-NUM2 = (COPY D6. + COPY D6.) * WS-TMP-NUM0.
      *    We vary the final result by up to half of the radius multi.
           COMPUTE WS-TMP-NUM1 = WS-TMP-NUM0 / 2.
           CALL 'ALTER-VALUE-BY-UPTO' USING
                                       WS-TMP-NUM1, WS-TMP-NUM2,
                                       LK-AVG-DISTANCE.
           GOBACK.
