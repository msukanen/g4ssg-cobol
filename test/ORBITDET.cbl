       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORBITAL-DETAILS-TEST.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Test orbital separation determining.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. WSL WITH DEBUGGING MODE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-RADIUS-MUL               PIC 9(2)V9(2) USAGE COMP-3.      AU
       01  D6                          PIC 99 USAGE COMP-3.
       01  NUM-OF-STARS                PIC 99 VALUE 3.
       01  IDX                         INDEX.
       01  STAR-ORBIT.
           05  ECCENTRICITY PIC 9V9(2) USAGE COMP-3.
           05  SEPARATION   PIC XX.
           05  AVG-RADIUS   PIC 9(5)V9(5) USAGE COMP-3.
           05  MIN-RADIUS   PIC 9(5)V9(5) USAGE COMP-3.
           05  MAX-RADIUS   PIC 9(5)V9(5) USAGE COMP-3.

       PROCEDURE DIVISION.
      D    DISPLAY 'Running STAR-ORBITAL-DETAILS 'NUM-OF-STARS' times.'
           PERFORM VARYING IDX FROM 1 BY 1
                   UNTIL IDX > NUM-OF-STARS
               CALL 'STAR-ORBITAL-DETAILS'
                   USING NUM-OF-STARS, IDX, STAR-ORBIT
      D        IF IDX > 1 THEN
      D            DISPLAY 'ECCENTRICITY: 'ECCENTRICITY
      D            DISPLAY 'SEPARATION  : 'SEPARATION
      D            DISPLAY 'MIN-RADIUS  : 'MIN-RADIUS
      D            DISPLAY 'AVG-RADIUS  : 'AVG-RADIUS
      D            DISPLAY 'MAX-RADIUS  : 'MAX-RADIUS
      D        END-IF
           END-PERFORM.
           STOP RUN.
