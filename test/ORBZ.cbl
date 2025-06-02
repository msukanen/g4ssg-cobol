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
       
       PROCEDURE DIVISION.
      D    DISPLAY '[test-orbital-zones]'
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
           CALL 'LOCATE-ORBITAL-ZONES' USING WS-STAR, ORBITAL-ZONES
      D    DISPLAY '   inner 'OZ-INNER-LIMIT
      D    DISPLAY '   outer 'OZ-OUTER-LIMIT
      D    DISPLAY 'snowline 'OZ-SNOW-LINE
           EXIT PARAGRAPH.

       I1I2.
           COMPUTE WS-I1 = 0.1 * MASS
           COMPUTE WS-L = LUMINOSITY
           COMPUTE WS-I2 = 0.01 * FUNCTION SQRT(WS-L)
           EXIT PARAGRAPH.
