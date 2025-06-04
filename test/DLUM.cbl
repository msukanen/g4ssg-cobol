       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-DETERMINE-LUMINOSITY.
       AUTHOR.     Markku Sukanen.
       COPY TESTENV.
      ******************************************************************
      *
      * Test luminosity.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CONST.
       01  WS-LUM                      PIC 9(12)V9 USAGE COMP-3.
       01  WS-TMP1                     USAGE COMP-2.
       01  WS-TMP2                     USAGE COMP-2.
       01  WS-EVO.
           COPY STLREVO.
       01  WS-STAGE                    PIC X(4).
           COPY STARSTG.
       01  WS-AGE                      PIC 99V99.
      *LINKAGE SECTION.
       01  LUMINOSITY                  PIC 9(12)V9(3) USAGE COMP-3.

       PROCEDURE DIVISION.
           MOVE 1.7 TO EVO-L-MIN
           MOVE 3.0 TO EVO-L-MAX
           MOVE 4.9 TO WS-AGE
           MOVE 5.9 TO EVO-M-SPAN
           SET CLASS-V TO TRUE
           CALL 'DETERMINE-LUMINOSITY' USING
                                       WS-EVO, WS-AGE, WS-STAGE,
                                       LUMINOSITY
           STOP RUN.
