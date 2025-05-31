       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-DETERMINE-SEQUENCE.
       AUTHOR.     Markku Sukanen.
       COPY TESTENV.
      ******************************************************************
      *
      * Test star's life sequence determining.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-AGE                      PIC 99V99.
       01  WS-EVO.
           COPY STLREVO.
       01  WS-STAGE                    PIC X(4).
           COPY STARSTG.
       
       PROCEDURE DIVISION.
           MOVE 3.0 TO WS-AGE.
           MOVE 5.2 TO EVO-M-SPAN
           MOVE 0.8 TO EVO-S-SPAN
           MOVE 0.5 TO EVO-G-SPAN
           CALL 'DETERMINE-SEQUENCE' USING WS-EVO, WS-AGE, WS-STAGE.
           IF NOT CLASS-V THEN
               MOVE 23 TO RETURN-CODE
               DISPLAY RETURN-CODE': class not V'
               STOP RUN.
           MOVE 6.0 TO WS-AGE.
           CALL 'DETERMINE-SEQUENCE' USING WS-EVO, WS-AGE, WS-STAGE.
           IF NOT CLASS-IV THEN
               MOVE 29 TO RETURN-CODE
               DISPLAY RETURN-CODE': class not IV'
               STOP RUN.
           MOVE 6.01 TO WS-AGE.
           CALL 'DETERMINE-SEQUENCE' USING WS-EVO, WS-AGE, WS-STAGE.
           IF NOT CLASS-III THEN
               MOVE 35 TO RETURN-CODE
               DISPLAY RETURN-CODE': class not III'
               STOP RUN.
           MOVE 6.6 TO WS-AGE.
           CALL 'DETERMINE-SEQUENCE' USING WS-EVO, WS-AGE, WS-STAGE.
           IF NOT CLASS-D THEN
               MOVE 41 TO RETURN-CODE
               DISPLAY RETURN-CODE': class not D'
               STOP RUN.
           STOP RUN.
