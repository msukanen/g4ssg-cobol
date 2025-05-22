       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORBITAL-SEPARATION.
       AUTHOR.     Markku Sukanen
      **************************************************************************
      * Calculate orbital separation between two stars.
      **************************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SEP                      PIC XX.
       01  WS-RADIUS-MUL               PIC 9(2)V9(2) USAGE COMP-3.      AU
       01  WS-AVG-RADIUS               PIC 9(3)V9(2) USAGE COMP-3.      AU
       01  WS-R                        PIC 99 USAGE COMP-3.
       
       PROCEDURE DIVISION.
           CALL '2D6' USING WS-R.
           EVALUATE TRUE
               WHEN WS-R <= 6
                   MOVE 'VC' TO WS-SEP
                   MOVE 0.05 TO WS-RADIUS-MUL
               WHEN WS-R <= 9
                   MOVE 'C' TO WS-SEP
                   MOVE 0.5 TO WS-RADIUS-MUL
               WHEN WS-R <= 11
                   MOVE 'M' TO WS-SEP
                   MOVE 2.0 TO WS-RADIUS-MUL
               WHEN WS-R <= 14
                   MOVE 'W' TO WS-SEP
                   MOVE 10.0 TO WS-RADIUS-MUL
               WHEN OTHER
                   MOVE 'D' TO WS-SEP
                   MOVE 50.0 TO WS-RADIUS-MUL
           END-EVALUATE.
           COMPUTE WS-AVG-RADIUS = WS-RADIUS-MUL * WS-R
           GOBACK.
