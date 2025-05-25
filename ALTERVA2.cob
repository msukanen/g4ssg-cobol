       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER-VALUE-BY-PERCENTAGE.
       AUTHOR.     Markku Sukanen

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHANGE          PIC 9(5)V9(5) USAGE COMP-3.
       01  WS-MUL             PIC 9(5)V9(5) USAGE COMP-3.
       
       LINKAGE SECTION.
       01  VAR-PERCENTAGE     PIC 9(5)V9(5) USAGE COMP-3.
       01  VAR-VALUE          PIC 9(5)V9(5) USAGE COMP-3.

      *********
      * Alter the given value by up to some given percentage up/down.
      *
      * VAR-PERCENTAGE   +/-% of variance on
      * VAR-VALUE        some number.
      *
      *********
       PROCEDURE DIVISION USING VAR-PERCENTAGE, VAR-VALUE.
           COMPUTE WS-MUL = VAR-PERCENTAGE / 100.0
           COMPUTE WS-CHANGE = VAR-VALUE * WS-MUL
           CALL 'ALTER-VALUE-BY-UPTO' USING WS-CHANGE, VAR-VALUE
           GOBACK.
