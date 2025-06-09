       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALTER-VALUE-BY-UPTO.
       AUTHOR.     Markku Sukanen.
      *************
      * Alter the given value up/down by up to some other given value.
      *
      * LK-UPTO    +/- 0..X on
      * LK-SRC     some number
      * LK-RESULT  returned here.
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHANGE                   USAGE COMP-2.
       
       LINKAGE SECTION.
       01  LK-UPTO                     USAGE COMP-2.
       01  LK-SRC                      USAGE COMP-2.
       01  LK-RESULT                   USAGE COMP-2.

       PROCEDURE DIVISION USING LK-UPTO, LK-SRC, LK-RESULT.
           COMPUTE WS-CHANGE = FUNCTION RANDOM * LK-UPTO
           IF FUNCTION RANDOM < 0.5 THEN
               COMPUTE LK-RESULT = LK-SRC - WS-CHANGE
           ELSE
               COMPUTE LK-RESULT = LK-SRC + WS-CHANGE
           END-IF
           GOBACK.
