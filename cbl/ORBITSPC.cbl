       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORBITAL-SPACE-GAP.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Determine orbital spacing gap.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  D6                          PIC 99 USAGE COMP-3.
       01  UPTO                        PIC 9(5)V9(5) USAGE COMP-3.
       01  TMP-NUM                     PIC 9(5)V9(5) USAGE COMP-3.
       01  RATIO                       PIC 9(5)V9(5) USAGE COMP-3.
       LINKAGE SECTION.
       01  LK-DIRECTION                PIC X.
           88  INWARD                  VALUE 'I'.
           88  OUTWARD                 VALUE 'O'.
       01  LK-ORIG-DISTANCE            PIC 9(5)V9(5) USAGE COMP-3.         AU
       01  LK-DISTANCE-SHIFT.
           COPY DISTSHFT.
       
       PROCEDURE DIVISION USING LK-DIRECTION,
                                LK-ORIG-DISTANCE,
                                LK-DISTANCE-SHIFT.
           PERFORM DETERMINE-SPACING-RATIO.
      
      *    Ratio adjustment.
           MOVE 0.05 TO UPTO
           MOVE RATIO TO TMP-NUM
           CALL 'ALTER-VALUE-BY-UPTO' USING UPTO, TMP-NUM, RATIO
      D    DISPLAY 'SHIFT-RATIO 'RATIO
      
      *    Calculate new distance.
           IF INWARD THEN
               COMPUTE NEW-DISTANCE = LK-ORIG-DISTANCE / RATIO
           ELSE
               COMPUTE NEW-DISTANCE = LK-ORIG-DISTANCE * RATIO
           END-IF
           COMPUTE SHIFT-FROM-ORIGIN ROUNDED
                 = NEW-DISTANCE - LK-ORIG-DISTANCE
           GOBACK.

      *****
      * Orbital spacing ratio.
      *****
       DETERMINE-SPACING-RATIO.
           CALL '3D6' USING D6
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 4
                   MOVE 1.4 TO RATIO
               WHEN D6 IS LESS OR EQUAL TO 6
                   MOVE 1.5 TO RATIO
               WHEN D6 IS LESS OR EQUAL TO 8
                   MOVE 1.6 TO RATIO
               WHEN D6 IS LESS OR EQUAL TO 12
                   MOVE 1.7 TO RATIO
               WHEN D6 IS LESS OR EQUAL TO 14
                   MOVE 1.8 TO RATIO
               WHEN D6 IS LESS OR EQUAL TO 16
                   MOVE 1.9 TO RATIO
               WHEN OTHER
                   MOVE 2.0 TO RATIO
           END-EVALUATE
           EXIT.
