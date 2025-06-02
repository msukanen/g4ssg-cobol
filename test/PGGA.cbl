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
       01  WS-GGA                      PIC 9.
           COPY GGA.
       01  WS-XPGGA                    PIC X(79).

       PROCEDURE DIVISION.
           CALL 'DETERMINE-GAS-GIANT-ARRANGEMENT' USING WS-GGA
           DISPLAY ' GGA: 'WS-GGA' = ' NO ADVANCING
           CALL 'EXPLAIN-GAS-GIANT-ARRANGEMENT' USING WS-GGA, WS-XPGGA
           DISPLAY FUNCTION TRIM(WS-XPGGA)
           STOP RUN.
