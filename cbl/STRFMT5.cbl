       IDENTIFICATION DIVISION.
       PROGRAM-ID.   FMT-NUM.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 3, 2025
      ******************************************************************
      *
      * Format given 9(5)V9(5) value so that it has no leading or
      * trailing spaces/zeroes.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ED-STR                   PIC ZZZZ9.9(5).
       01  WS-FMT-STR                  PIC X(11).
       01  WS-I                        USAGE COMP-2.
       01  WS-L                        USAGE COMP-2.
       
       LINKAGE SECTION.
       01  LK-SRC-NUM                  PIC 9(5)V9(5) USAGE COMP-3.
       01  LK-RES-STR                  PIC X(11).

       PROCEDURE DIVISION USING LK-SRC-NUM, LK-RES-STR.
           COPY STRFMT.
           GOBACK.
       