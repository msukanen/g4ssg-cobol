       IDENTIFICATION DIVISION.
       PROGRAM-ID.   ALTER-VALUE-BY-PERCENTAGE.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. ? - June 9, 2025.
      *********
      * Alter the given value by up to some given percentage up/down.
      *
      * LK-PERCENTAGE  +/-% of variance on
      * LK-SRC         some number
      * LK-RESULT      returned here.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHANGE                   USAGE COMP-2.
       01  WS-MUL                      USAGE COMP-2.
       
       LINKAGE SECTION.
       01  LK-PERCENTAGE               USAGE COMP-2.
       01  LK-SRC                      USAGE COMP-2.
       01  LK-RESULT                   USAGE COMP-2.

       PROCEDURE DIVISION USING LK-PERCENTAGE, LK-SRC, LK-RESULT.
           COMPUTE WS-MUL = LK-PERCENTAGE / 100.0
           COMPUTE WS-CHANGE = LK-SRC * WS-MUL
           CALL 'ALTER-VALUE-BY-UPTO' USING WS-CHANGE, LK-SRC, LK-RESULT
           GOBACK.
