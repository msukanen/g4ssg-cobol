       IDENTIFICATION DIVISION.
       PROGRAM-ID. STAR-ORBIT-LIMITS.
       AUTHOR.     Markku Sukanen

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM1                        PIC 9(5)V9(5) USAGE COMP-3.
       01  NUM2                        PIC 9(5)V9(5) USAGE COMP-3.
       01  WS-STAR.
           05  WS-INNER                PIC 9(5)V9(5) USAGE COMP-3.
           05  WS-OUTER                PIC 9(5)V9(5) USAGE COMP-3.
           05  WS-SNOW                 PIC 9(5)V9(5) USAGE COMP-3.

       LINKAGE SECTION.
       01  MASS                        PIC 9(5)V9(5) USAGE COMP-3.
       01  LUMINOSITY                  PIC 9(5)V9(5) USAGE COMP-3.
       01  ORBITAL-LIMITS.
           05  INNER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3.      AU
           05  OUTER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3.      AU
           05  SNOW-LINE               PIC 9(5)V9(5) USAGE COMP-3.      AU

       PROCEDURE DIVISION USING MASS, LUMINOSITY, ORBITAL-LIMITS.
      D    DISPLAY 'STAR-ORBIT-LIMITS:'
      *D    DISPLAY '  MASS 'MASS
      *D    DISPLAY '  LUM  'LUMINOSITY
           COMPUTE NUM1 = MASS / 10.0.
           COMPUTE NUM2 ROUNDED = FUNCTION SQRT(LUMINOSITY) / 100.0

           IF NUM1 > NUM2 THEN
               MOVE NUM1 TO WS-INNER
           ELSE
               MOVE NUM2 TO WS-INNER
           END-IF
      D    DISPLAY '  inner     : 'WS-INNER' AU'

           COMPUTE WS-OUTER = MASS * 40.0
      D    DISPLAY '  outer     : 'WS-OUTER' AU'
           COMPUTE WS-SNOW ROUNDED = FUNCTION SQRT(LUMINOSITY) * 4.85
      D    DISPLAY '  snow-line : 'WS-SNOW' AU'
           MOVE WS-INNER TO INNER-LIMIT
           MOVE WS-OUTER TO OUTER-LIMIT
           move WS-SNOW TO SNOW-LINE
           GOBACK.
