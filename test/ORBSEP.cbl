       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TEST-ORBITAL-SEPARATION.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 11, 2025.
      ******************************************************************
      *
      * Test orbital separation generation.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-THIRD-IN-TRINARY         PIC X VALUE IS '-'.
           88  THIRD-IN-TRINARY        VALUE 'Y'
                                       WHEN SET TO FALSE IS '-'.
       01  WS-LOOP                     PIC 9 USAGE COMP-5.
       01  STAR-COUNT                  PIC 999 USAGE COMP-5.            STARSEP!
       01  WS-SEP.
           COPY STARSEP.

       PROCEDURE DIVISION.
           MOVE 3 TO STAR-COUNT.
           PERFORM VARYING WS-LOOP     FROM 1 BY 1
                                       UNTIL WS-LOOP > STAR-COUNT
               IF WS-LOOP = 3 THEN SET THIRD-IN-TRINARY TO TRUE
               IF WS-LOOP = 1 THEN
                   CALL 'DETERMINE-ORBITAL-SEP' USING
                                       WS-THIRD-IN-TRINARY, STAR-COUNT,
                                       WS-SEP, OMITTED, SEP-IDX
               ELSE CALL 'DETERMINE-ORBITAL-SEP' USING
                                       WS-THIRD-IN-TRINARY,
                                       STAR-COUNT, WS-SEP,
                                       PREV-SEP-IDX, SEP-IDX
               END-IF
           END-PERFORM.
           STOP RUN.
