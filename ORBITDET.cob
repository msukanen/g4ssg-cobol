       IDENTIFICATION DIVISION.
       PROGRAM-ID. STAR-ORBITAL-DETAILS.
       AUTHOR.     Markku Sukanen
      **************************************************************************
      * Calculate orbital separation between two stars.
      **************************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RADIUS-MUL               PIC 9(2)V9(2) USAGE COMP-3.      AU
       01  D6                          PIC 99 USAGE COMP-3.

       LINKAGE SECTION.
       01  NUM-OF-STARS                PIC 99.
       01  IDX                         INDEX.
       01  STAR-ORBIT.
           05  ECCENTRICITY PIC 9V9(2) USAGE COMP-3.
           05  SEPARATION   PIC XX.
           05  AVG-RADIUS   PIC 9(5)V9(5) USAGE COMP-3.
           05  MIN-RADIUS   PIC 9(5)V9(5) USAGE COMP-3.
           05  MAX-RADIUS   PIC 9(5)V9(5) USAGE COMP-3.
       
       PROCEDURE DIVISION
           USING   NUM-OF-STARS,
                   IDX,
                   STAR-ORBIT.
           IF NUM-OF-STARS < 2 OR IDX = 1 THEN
      D        DISPLAY '<note> STAR-ORBITAL-DETAILS unnecessary for '
                       'single star and/or star-index 1.'
               MOVE 'NA' TO SEPARATION
               MOVE 0.0 TO ECCENTRICITY
               MOVE 0.0 TO AVG-RADIUS
               MOVE 0.0 TO MIN-RADIUS
               MOVE 0.0 TO MAX-RADIUS
               GOBACK
           END-IF.

           CALL '2D6' USING D6
           EVALUATE TRUE
               WHEN D6 <= 6
                   MOVE 'VC' TO SEPARATION
                   MOVE 0.05 TO WS-RADIUS-MUL
               WHEN D6 <= 9
                   MOVE 'C' TO SEPARATION
                   MOVE 0.5 TO WS-RADIUS-MUL
               WHEN D6 <= 11
                   MOVE 'M' TO SEPARATION
                   MOVE 2.0 TO WS-RADIUS-MUL
               WHEN D6 <= 14
                   MOVE 'W' TO SEPARATION
                   MOVE 10.0 TO WS-RADIUS-MUL
               WHEN OTHER
                   MOVE 'D' TO SEPARATION
                   MOVE 50.0 TO WS-RADIUS-MUL
           END-EVALUATE.
           
           COMPUTE AVG-RADIUS = WS-RADIUS-MUL * D6.

           CALL '3D6' USING D6.
           IF IDX > 2 THEN
               COMPUTE D6 = D6 + 6
           END-IF.

           EVALUATE TRUE
               WHEN D6 <= 3
                   MOVE 0.0 TO ECCENTRICITY
               WHEN D6 = 4
                   MOVE 0.1 TO ECCENTRICITY
               WHEN D6 = 5
                   MOVE 0.2 TO ECCENTRICITY
               WHEN D6 = 6
                   MOVE 0.3 TO ECCENTRICITY
               WHEN D6 <= 8
                   MOVE 0.4 TO ECCENTRICITY
               WHEN D6 <= 11
                   MOVE 0.5 TO ECCENTRICITY
               WHEN D6 <= 13
                   MOVE 0.6 TO ECCENTRICITY
               WHEN D6 <= 15
                   MOVE 0.7 TO ECCENTRICITY
               WHEN D6 = 16
                   MOVE 0.8 TO ECCENTRICITY
               WHEN D6 = 17
                   MOVE 0.9 TO ECCENTRICITY
               WHEN OTHER
                   MOVE 0.95 TO ECCENTRICITY
           END-EVALUATE.

           COMPUTE MIN-RADIUS = (1.0 - ECCENTRICITY) * AVG-RADIUS.
           COMPUTE MAX-RADIUS = (1.0 + ECCENTRICITY) * AVG-RADIUS.
           GOBACK.
