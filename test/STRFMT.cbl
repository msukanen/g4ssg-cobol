       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TEST-STRING-FORMATTING.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 3, 2025.
       COPY          TESTENV.
      ******************************************************************
      *
      * Testing various (or not so various) string formatting wannabes.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SOURCE-NUM               USAGE COMP-2.
       01  WS-RES-STR                  PIC X(11).
       01  WS-DIGITS                   PIC S9.

       PROCEDURE DIVISION.
      D    DISPLAY 'Figuring out how to do string' NO ADVANCING
      D    DISPLAY ' formatting in COBOL…'
      D    DISPLAY SPACE
      D    DISPLAY '      TEST: 0.07500'
           MOVE 0.075 TO WS-SOURCE-NUM
           MOVE 3 TO WS-DIGITS
           CALL 'FMT-NUM' USING WS-SOURCE-NUM, WS-DIGITS, WS-RES-STR
           IF '0.075' <> FUNCTION TRIM(WS-RES-STR) THEN
      D        DISPLAY 'ERROR: 0.075 ≠ "'
      D                FUNCTION TRIM(WS-RES-STR)'"'
               MOVE 112 TO RETURN-CODE
               STOP RUN
           END-IF
      D    DISPLAY '      OK-3: ' FUNCTION TRIM(WS-RES-STR)

      D    DISPLAY SPACE
      D    DISPLAY '      TEST: 0.07599'
           MOVE 0.07599 TO WS-SOURCE-NUM
           MOVE 5 TO WS-DIGITS
           CALL 'FMT-NUM' USING WS-SOURCE-NUM, WS-DIGITS, WS-RES-STR
           IF '0.07599' <> FUNCTION TRIM(WS-RES-STR) THEN
      D        DISPLAY 'ERROR: 0.07599 ≠ "'
      D                FUNCTION TRIM(WS-RES-STR)'"'
               MOVE 112 TO RETURN-CODE
               STOP RUN
           END-IF
      D    DISPLAY '      OK-5: ' FUNCTION TRIM(WS-RES-STR)

           STOP RUN.
