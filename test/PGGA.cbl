       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-GAS-GIANT-ARRANGEMENT.
       AUTHOR.     Markku Sukanen.
       COPY TESTENV.
      ******************************************************************
      *
      * Test gas giant arrangement.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STAR.
           COPY STAR.
       01  WS-XPGGA                    PIC X(79).
       01  X                           USAGE COMP-1.
       77  TIMES-TO-TEST               USAGE COMP-1 VALUE 1000.

       PROCEDURE DIVISION.
           PERFORM VARYING X FROM 1 BY 1 UNTIL X > TIMES-TO-TEST
               CALL 'DETERMINE-GAS-GIANT-ARRANGEMENT' USING WS-STAR
               IF GG-ARRANGEMENT < 1 OR GG-ARRANGEMENT > 4 THEN
                   DISPLAY 'Something is awfully wrong with GGA...'
                           NO ADVANCING
                   DISPLAY ' or the dev(s)!'
                   MOVE 112 TO RETURN-CODE
                   STOP RUN
               END-IF
               DISPLAY ' GGA: 'GG-ARRANGEMENT' = ' NO ADVANCING
               CALL 'EXPLAIN-GAS-GIANT-ARRANGEMENT'
                                       USING GG-ARRANGEMENT, WS-XPGGA
               DISPLAY FUNCTION TRIM(WS-XPGGA)
           END-PERFORM
           SUBTRACT 1 FROM X GIVING X
      D    DISPLAY X ' tests completed.'
           STOP RUN.
