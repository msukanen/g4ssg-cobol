       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-ORBITAL-SEPARATION.
       AUTHOR.     Markku Sukanen.
       COPY TESTENV.
      ******************************************************************
      *
      * Test orbital separation.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-3RD-OF-TRINARY           PIC X VALUE 'N'.
       01  WS-ORBITAL.
           COPY ORBSEPV.
       
       PROCEDURE DIVISION.
           DISPLAY '[orbital-info]'
           CALL 'DETERMINE-ORBITAL-SEPARATION' USING
                                       WS-3RD-OF-TRINARY, WS-ORBITAL
           DISPLAY '  ecc 'SEP-ECC
           DISPLAY '  sep 'SEP-MIN'—'SEP-AVG'—'SEP-MAX
           STOP RUN.
