       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER-VALUE-BY-UPTO.
       AUTHOR.     Markku Sukanen.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHANGE                   USAGE COMP-2.
       
       LINKAGE SECTION.
       01  VAR-BY-UPTO                 USAGE COMP-2.
       01  VAR-VALUE                   USAGE COMP-2.
       01  RET-VALUE                   USAGE COMP-2.

      *********
      * Alter the given value by up to some given percentage up/down.
      *
      * VAR-PERCENTAGE   +/-% of variance on
      * VAR-VALUE        some number
      * RET-VALUE        returned here.
      *
      *********
       PROCEDURE DIVISION USING VAR-BY-UPTO, VAR-VALUE, RET-VALUE.
           COMPUTE WS-CHANGE = FUNCTION RANDOM * VAR-BY-UPTO
      *    DISPLAY '[alter-value-by-upto]'
      *            ' ±[0..' VAR-BY-UPTO']'
      *            ' → ' WS-CHANGE
      *            ' of ' VAR-VALUE NO ADVANCING
           IF FUNCTION RANDOM < 0.5 THEN
               COMPUTE RET-VALUE = VAR-VALUE - WS-CHANGE
           ELSE
               COMPUTE RET-VALUE = VAR-VALUE + WS-CHANGE
           END-IF
      *    DISPLAY ' = 'RET-VALUE
           GOBACK.
