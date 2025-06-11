       IDENTIFICATION DIVISION.
       PROGRAM-ID.   FMT-NUM.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 3—10, 2025
      ******************************************************************
      *
      * Format given source value so that it has no leading or trailing
      * spaces/zeroes.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM5                     PIC 9(15)V9(5).
       01  WS-ED-STR5                  PIC Z(14)9.9(5).
       01  WS-NUM4                     PIC 9(15)V9(4).
       01  WS-ED-STR4                  PIC Z(14)9.9(4).
       01  WS-NUM3                     PIC 9(15)V9(3).
       01  WS-ED-STR3                  PIC Z(14)9.9(3).
       01  WS-NUM2                     PIC 9(15)V9(2).
       01  WS-ED-STR2                  PIC Z(14)9.9(2).
       01  WS-NUM1                     PIC 9(15)V9(1).
       01  WS-ED-STR1                  PIC Z(14)9.9(1).
       01  WS-NUM0                     PIC 9(15).
       01  WS-ED-STR0                  PIC Z(14)9.
       01  WS-FMT-STR                  PIC X(20).
       01  WS-I                        USAGE COMP-2.
       01  WS-L                        USAGE COMP-2.
       
       LINKAGE SECTION.
       01  LK-SRC-NUM                  USAGE COMP-2.
       01  LK-RES-STR                  PIC X(11).
       01  LK-DIGITS                   PIC S9.

       PROCEDURE DIVISION USING LK-SRC-NUM, LK-DIGITS, LK-RES-STR.
           EVALUATE TRUE
               WHEN LK-DIGITS IS OMITTED OR LK-DIGITS >= 5
                   COMPUTE WS-NUM5 ROUNDED = LK-SRC-NUM
                   MOVE WS-NUM5 TO WS-ED-STR5
                   INSPECT WS-ED-STR5 TALLYING WS-L FOR LEADING SPACE
                   MOVE FUNCTION REVERSE(WS-ED-STR5) TO WS-FMT-STR
               WHEN LK-DIGITS = 4
                   COMPUTE WS-NUM4 ROUNDED = LK-SRC-NUM
                   MOVE WS-NUM4 TO WS-ED-STR4
                   INSPECT WS-ED-STR4 TALLYING WS-L FOR LEADING SPACE
                   MOVE FUNCTION REVERSE(WS-ED-STR4) TO WS-FMT-STR
               WHEN LK-DIGITS = 3
                   COMPUTE WS-NUM3 ROUNDED = LK-SRC-NUM
                   MOVE WS-NUM3 TO WS-ED-STR3
                   INSPECT WS-ED-STR3 TALLYING WS-L FOR LEADING SPACE
                   MOVE FUNCTION REVERSE(WS-ED-STR3) TO WS-FMT-STR
               WHEN LK-DIGITS = 2
                   COMPUTE WS-NUM2 ROUNDED = LK-SRC-NUM
                   MOVE WS-NUM2 TO WS-ED-STR2
                   INSPECT WS-ED-STR2 TALLYING WS-L FOR LEADING SPACE
                   MOVE FUNCTION REVERSE(WS-ED-STR2) TO WS-FMT-STR
               WHEN LK-DIGITS = 1
                   COMPUTE WS-NUM1 ROUNDED = LK-SRC-NUM
                   MOVE WS-NUM1 TO WS-ED-STR1
                   INSPECT WS-ED-STR1 TALLYING WS-L FOR LEADING SPACE
                   MOVE FUNCTION REVERSE(WS-ED-STR1) TO WS-FMT-STR
               WHEN LK-DIGITS <= 0
                   COMPUTE WS-NUM0 ROUNDED = LK-SRC-NUM
                   MOVE WS-NUM0 TO WS-ED-STR0
                   INSPECT WS-ED-STR0 TALLYING WS-L FOR LEADING SPACE
                   MOVE FUNCTION REVERSE(WS-ED-STR0) TO WS-FMT-STR
           END-EVALUATE

           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-L
               IF WS-FMT-STR(WS-I:1) = '0' THEN
                   MOVE SPACE TO WS-FMT-STR(WS-I:1)
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM.

           INITIALIZE LK-RES-STR
           STRING FUNCTION TRIM(WS-FMT-STR)
                  DELIMITED BY SIZE INTO LK-RES-STR
           MOVE FUNCTION REVERSE(LK-RES-STR) TO LK-RES-STR

           GOBACK.
