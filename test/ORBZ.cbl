       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ORBITAL-ZONES.
       AUTHOR.     Markku Sukanen.
       COPY TESTENV.
      ******************************************************************
      *
      * Testing orbital zones.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STAR.
           COPY STAR.
       01  WS-I1                       USAGE COMP-2.
       01  WS-I2                       USAGE COMP-2.
       01  WS-L                        USAGE COMP-2.
       01  WS-COUNT                    USAGE COMP-1 VALUE 0.
       01  WS-STR                      PIC X(11).
       01  DISPLAY-STR                 PIC X(79).
       COPY ANSI.
       
       PROCEDURE DIVISION.
           STRING ANSI-ESC ANSI-GREEN
                  '[test-orbital-zones]'
                  ANSI-ESC ANSI-RESET
                  DELIMITED BY SIZE
                  INTO DISPLAY-STR
           DISPLAY FUNCTION TRIM(DISPLAY-STR)
           MOVE 1.0 TO MASS
           MOVE 1.0 TO LUMINOSITY
           PERFORM I1I2
      * Lets keep going until luminosity pushes limits further than mass
      * alone would do.
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-I2 > WS-I1
               IF WS-COUNT > 1 THEN
                   DISPLAY '________ _____._____'
                   END-IF
               DISPLAY '    mass 'MASS
               DISPLAY '     lum 'LUMINOSITY
               PERFORM LOCATE
               COMPUTE MASS ROUNDED = 1.2 * MASS
               IF WS-COUNT = 1 THEN
                   COMPUTE LUMINOSITY ROUNDED = 1.44 * LUMINOSITY
               ELSE
                   COMPUTE LUMINOSITY ROUNDED = LUMINOSITY ** 2
               END-IF
               PERFORM I1I2
           END-PERFORM.
      D    DISPLAY '  OK: 'WS-COUNT' iterations done.'
           STOP RUN.

       LOCATE.
           CALL 'PLACE-ORBITAL-ZONES' USING WS-STAR, ORBITAL-ZONES
      D    CALL 'FMT-NUM' USING OZ-INNER-LIMIT, WS-STR
      D    DISPLAY '   inner ' FUNCTION TRIM(WS-STR) ' AU'
      D    CALL 'FMT-NUM' USING OZ-OUTER-LIMIT, WS-STR
      D    DISPLAY '   outer ' FUNCTION TRIM(WS-STR) ' AU'
      D    CALL 'FMT-NUM' USING OZ-SNOW-LINE, WS-STR
      D    DISPLAY 'snowline ' FUNCTION TRIM(WS-STR) ' AU'
           EXIT PARAGRAPH.

       I1I2.
           COMPUTE WS-I1 = 0.1 * MASS
           COMPUTE WS-L = LUMINOSITY
           COMPUTE WS-I2 = 0.01 * FUNCTION SQRT(WS-L)
           EXIT PARAGRAPH.
