       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER-VALUE-BY-UPTO.
       AUTHOR.     Markku Sukanen.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHANGE                   USAGE COMP-1.
       
       LINKAGE SECTION.
       01  VAR-BY-UPTO                 PIC 9(5)V9(5) USAGE COMP-3.
       01  VAR-VALUE                   PIC 9(5)V9(5) USAGE COMP-3.
       01  RET-VALUE                   PIC 9(5)V9(5) USAGE COMP-3.

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
           IF FUNCTION RANDOM < 0.5 THEN
               COMPUTE RET-VALUE ROUNDED = VAR-VALUE - WS-CHANGE
           ELSE
               COMPUTE RET-VALUE ROUNDED = VAR-VALUE + WS-CHANGE
           END-IF
           GOBACK.
