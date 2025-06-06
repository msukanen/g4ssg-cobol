       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-ORBIT-SPACING.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 4, 2025
      ****************************************************************** p.108-
      *                                                                    109
      * Determine orbit spacing.
      *
      * For increased accuracy we use COMP-2 instead of 9(5)v9(5) for
      * distance computations.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RATIO                    USAGE COMP-2.
       01  D6                          PIC 99 USAGE COMP-3.
       01  WS-DIFF                     USAGE COMP-2.
       77  RATIO-SHIFT-UPTO            USAGE COMP-2 VALUE 0.05.

       LINKAGE SECTION.
       01  LK-DIR                      PIC X.
           COPY ORBDIR.
       01  LK-ORBZONE.
           COPY ORBZONE.
       01  LK-SRC-DISTANCE             USAGE COMP-2.
       01  LK-DST-DISTANCE             USAGE COMP-2.
       01  LK-DST-AVAILABLE            PIC X.
           COPY ORBAVAIL.
      D01  LK-D-RATIO                  USAGE COMP-2.

       PROCEDURE DIVISION USING
                   LK-DIR,
                   LK-ORBZONE,
                   LK-SRC-DISTANCE,
                   LK-DST-AVAILABLE,
                   LK-DST-DISTANCE
      D            LK-D-RATIO
                   .
           PERFORM DETERMINE-RATIO.

           IF ODIR-INWARD THEN
               COMPUTE LK-DST-DISTANCE = LK-SRC-DISTANCE / WS-RATIO
           ELSE
               COMPUTE LK-DST-DISTANCE = LK-SRC-DISTANCE * WS-RATIO
           END-IF

           IF  LK-DST-DISTANCE < OZ-INNER-LIMIT
               OR LK-DST-DISTANCE > OZ-OUTER-LIMIT
               THEN SET DST-NOT-AVAILABLE TO TRUE
           ELSE
               SET DST-AVAILABLE TO TRUE
               IF ODIR-INWARD THEN
                   COMPUTE WS-DIFF = LK-SRC-DISTANCE - LK-DST-DISTANCE
      *            Orbits are placed at least 0.15 AU apart.  If this
      *            pushes the orbit too close to the star then mark it
      *            unavailable.
                   IF WS-DIFF < 0.15 THEN                               p.109
                       COMPUTE LK-DST-DISTANCE = LK-SRC-DISTANCE - 0.15
                       IF LK-DST-DISTANCE < OZ-INNER-LIMIT THEN
                           SET DST-NOT-AVAILABLE TO TRUE
                       END-IF
                   END-IF
               END-IF
           END-IF
           GOBACK.

      *************
      * Determine ratio (generally 1.4 to 2.0 ±0.05) of distance shift.
      *
       DETERMINE-RATIO.                                                 p.109
           CALL '3D6' USING D6
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 4
                   MOVE 1.4 TO WS-RATIO
               WHEN D6 IS LESS OR EQUAL TO 6
                   MOVE 1.5 TO WS-RATIO
               WHEN D6 IS LESS OR EQUAL TO 8
                   MOVE 1.6 TO WS-RATIO
               WHEN D6 IS LESS OR EQUAL TO 12
                   MOVE 1.7 TO WS-RATIO
               WHEN D6 IS LESS OR EQUAL TO 14
                   MOVE 1.8 TO WS-RATIO
               WHEN D6 IS LESS OR EQUAL TO 16
                   MOVE 1.9 TO WS-RATIO
               WHEN OTHER
                   MOVE 2.0 TO WS-RATIO
           END-EVALUATE
           
           CALL 'ALTER-VALUE-BY-UPTO' USING
                   RATIO-SHIFT-UPTO,
                   WS-RATIO, WS-DIFF
           COMPUTE WS-RATIO = WS-RATIO + WS-DIFF
      D    MOVE WS-RATIO TO LK-D-RATIO
           DISPLAY 'WS-RATIO 'WS-RATIO
           EXIT PARAGRAPH.
