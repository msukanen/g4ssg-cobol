       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TEST-ORBITAL-SPACING.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 4, 2025
       COPY TESTENV.
      ******************************************************************
      *
      * Test orbital spacing/orbit gaps.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DIR                      PIC X.
           COPY ORBDIR.
       01  WS-ORBZONE.
           COPY ORBZONE.
       01  WS-ORG-DISTANCE             USAGE COMP-2.
       01  WS-SRC-DISTANCE             USAGE COMP-2.
       01  WS-DST-DISTANCE             USAGE COMP-2.
       01  WS-DST-AVAILABLE            PIC X.
           COPY ORBAVAIL.
       01  WS-H-STR                    PIC X(11).
       01  WS-H-DISTANCE               PIC 9(5)V9(5) COMP-3.
      D01  WS-D-RATIO                  USAGE COMP-2.

       PROCEDURE DIVISION.
      *    Some initializations…
           MOVE 1.0 TO WS-ORG-DISTANCE
           MOVE 0.25 TO OZ-INNER-LIMIT
           MOVE 48.5 TO OZ-OUTER-LIMIT

           DISPLAY '---[ INWARD ]---'
           SET ODIR-INWARD TO TRUE
           SET DST-AVAILABLE TO TRUE
           MOVE WS-ORG-DISTANCE TO WS-SRC-DISTANCE
           PERFORM CREATE-ORBIT-SPREAD

           DISPLAY '---[ CENTER ]---'
           DISPLAY 'WS-H-DISTANCE '
                   FUNCTION TRIM(WS-H-DISTANCE)

           DISPLAY '---[ OUTWARD ]---'
           SET ODIR-OUTWARD TO TRUE
           SET DST-AVAILABLE TO TRUE
           MOVE WS-ORG-DISTANCE TO WS-SRC-DISTANCE
           PERFORM CREATE-ORBIT-SPREAD
           
           STOP RUN.

       CREATE-ORBIT-SPREAD.
           PERFORM UNTIL DST-NOT-AVAILABLE
               CALL 'DETERMINE-ORBIT-SPACING' USING
                                       WS-DIR, WS-ORBZONE,
                                       WS-SRC-DISTANCE,
                                       WS-DST-AVAILABLE,
                                       WS-DST-DISTANCE
      D                                , WS-D-RATIO
               IF DST-AVAILABLE THEN
                   MOVE WS-DST-DISTANCE TO WS-H-DISTANCE
                   CALL 'FMT-NUM' USING WS-H-DISTANCE, WS-H-STR
                   DISPLAY 'WS-H-DISTANCE ' FUNCTION TRIM(WS-H-STR)
      D                                NO ADVANCING
      D            DISPLAY ', D-RATIO 'WS-D-RATIO
                   MOVE WS-DST-DISTANCE TO WS-SRC-DISTANCE
               END-IF
           END-PERFORM
           EXIT PARAGRAPH.
