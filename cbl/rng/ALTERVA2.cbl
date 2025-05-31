       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER-VALUE-BY-PERCENTAGE.
       AUTHOR.     Markku Sukanen.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHANGE                   USAGE COMP-2.
       01  WS-MUL                      USAGE COMP-2.
       
       LINKAGE SECTION.
       01  VAR-PERCENTAGE              USAGE COMP-2.
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
       PROCEDURE DIVISION USING VAR-PERCENTAGE, VAR-VALUE, RET-VALUE.
           COMPUTE WS-MUL = VAR-PERCENTAGE / 100.0
           COMPUTE WS-CHANGE = VAR-VALUE * WS-MUL
           CALL 'ALTER-VALUE-BY-UPTO'
               USING WS-CHANGE,
                     VAR-VALUE,
                     RET-VALUE
      D    DISPLAY '[alter-value-by-percentage]'
      D    DISPLAY ' ±'VAR-PERCENTAGE'%' NO ADVANCING
      D    DISPLAY ' of 'VAR-VALUE' → 'RET-VALUE
           GOBACK.
