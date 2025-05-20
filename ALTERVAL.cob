       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER-VALUE-BY-PERCENTAGE.
       AUTHOR.     Markku Sukanen

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHANGE          PIC 9(5)V9(5) USAGE COMP-3 VALUE 1.0.
       01  WS-MUL             PIC 9(5)V9(5) USAGE COMP-3 VALUE 0.01.
       
       LINKAGE SECTION.
       01  VAR-PERCENTAGE     PIC 9(5)V9(5) USAGE COMP-3.
       01  VAR-VALUE          PIC 9(5)V9(5) USAGE COMP-3.
       01  RET-VAL            PIC 9(5)V9(5) USAGE COMP-3.

      *********
      * Calculate (random) percentage variance and such things.
      *
      * VAR-PERCENTAGE   +/-% of variance on
      * VAR-VALUE        some number.
      *
      *> Example:
      *    CALL 'VALUE-VARIANCE-BY' USING 10, 100.0, STORAGE-VAR
      *
      *    Will return a value between 90.0 and 110.0 in STORAGE-VAR.
      *********
       PROCEDURE DIVISION USING VAR-PERCENTAGE, VAR-VALUE, RET-VAL.
           COMPUTE WS-CHANGE = FUNCTION RANDOM * WS-MUL * VAR-PERCENTAGE
           IF FUNCTION RANDOM < 0.5 THEN
               COMPUTE WS-CHANGE = 1.0 - WS-CHANGE
           ELSE
               COMPUTE WS-CHANGE = 1.0 + WS-CHANGE
           END-IF
           COMPUTE RET-VAL = VAR-VALUE * WS-CHANGE
           GOBACK.
