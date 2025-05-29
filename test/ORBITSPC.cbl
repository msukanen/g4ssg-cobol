       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ORBIT-SPACING.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Test orbit spacing functionality.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. WSL WITH DEBUGGING MODE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-DIRECTION                PIC X.
       01  WS-ROOT-DISTANCE            PIC 9(5)V9(5) USAGE COMP-3
                                           VALUE 1.0.
       01  WS-ORIGIN-DISTANCE          PIC 9(5)V9(5) USAGE COMP-3
                                           VALUE 1.0.
       01  TMP-STR                     PIC ZZZZ9.999.
       01  WS-ORBIT-LIMITS.
           05  INNER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3
                                           VALUE 0.075.
           05  OUTER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3
                                           VALUE 40.
           05  SNOW-LINE               PIC 9(5)V9(5) USAGE COMP-3.       IGNORE
       01  WS-DISTANCE.
           05  AU                      PIC 9(5)V9(5) USAGE COMP-3.
           05  SHIFT                   PIC S9(5)V9(5) USAGE COMP-3.
       
       PROCEDURE DIVISION.
      D    DISPLAY '*  AU  'WS-ORIGIN-DISTANCE
      D    DISPLAY '* INNER 'INNER-LIMIT
      * Inward orbits first.
           MOVE 'I' TO WS-DIRECTION
      * Initial shift is zero, of course, so we straight to next orbit.
           PERFORM GAP-CALL
           PERFORM VARYING WS-ORIGIN-DISTANCE
                   FROM WS-ORIGIN-DISTANCE BY SHIFT
                   UNTIL AU < INNER-LIMIT
               PERFORM DISPLAY-SHIFT
               PERFORM GAP-CALL
           END-PERFORM
      D    DISPLAY '----------------',
                   'Discarded data:',
                   '  SHIFT 'SHIFT
           MOVE INNER-LIMIT TO TMP-STR
           MOVE FUNCTION TRIM(TMP-STR LEADING) TO TMP-STR
      D    DISPLAY '     AU  'AU
                   ' which would have been closer than '
                   FUNCTION TRIM(TMP-STR)' AU'
      * Outward this time:
      D    DISPLAY '*  AU  'WS-ORIGIN-DISTANCE
      D    DISPLAY '* OUTER 'OUTER-LIMIT
           MOVE 'O' TO WS-DIRECTION
           MOVE WS-ROOT-DISTANCE TO WS-ORIGIN-DISTANCE
           PERFORM GAP-CALL
           PERFORM VARYING WS-ORIGIN-DISTANCE
                   FROM WS-ORIGIN-DISTANCE BY SHIFT
                   UNTIL AU > OUTER-LIMIT
               PERFORM DISPLAY-SHIFT
               PERFORM GAP-CALL
           END-PERFORM
      D    DISPLAY '----------------',
                   'Discarded data:',
                   '  SHIFT 'SHIFT
           MOVE OUTER-LIMIT TO TMP-STR
           MOVE FUNCTION TRIM(TMP-STR LEADING) TO TMP-STR
      D    DISPLAY '     AU  'AU
                   ' which would have been farther than '
                   FUNCTION TRIM(TMP-STR)' AU'

           STOP RUN.

       GAP-CALL.
           CALL 'ORBITAL-SPACE-GAP'
               USING WS-DIRECTION,
                     WS-ORIGIN-DISTANCE,
                     WS-DISTANCE.
           EXIT.

       DISPLAY-SHIFT.
           DISPLAY '-----------'
           DISPLAY 'SHIFT      'SHIFT
           DISPLAY '   AU       'AU
           EXIT.
