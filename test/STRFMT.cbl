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
       01  WS-SOURCE-NUM               PIC 9(5)V9(5) USAGE COMP-3.
       01  WS-RES-STR                  PIC X(11).

       PROCEDURE DIVISION.
      D    DISPLAY 'Figuring out how to do string' NO ADVANCING
      D    DISPLAY ' formatting in COBOL…'
      D    DISPLAY SPACE
      D    DISPLAY '      TEST: 0.07500'
           MOVE 0.075 TO WS-SOURCE-NUM
           CALL 'FMT-NUM' USING WS-SOURCE-NUM, WS-RES-STR
           IF '0.075' <> FUNCTION TRIM(WS-RES-STR) THEN
               MOVE 112 TO RETURN-CODE
               STOP RUN
           END-IF

      D    DISPLAY SPACE
      D    DISPLAY '      TEST: 0.07599'
           MOVE 0.07599 TO WS-SOURCE-NUM
           CALL 'FMT-NUM' USING WS-SOURCE-NUM, WS-RES-STR
           IF '0.07599' <> FUNCTION TRIM(WS-RES-STR) THEN
               MOVE 112 TO RETURN-CODE
               STOP RUN
           END-IF
           STOP RUN.
