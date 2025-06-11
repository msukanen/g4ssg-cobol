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
       01  WS-ED-STR                   PIC Z(14)9.9(5).                  STRFMT
       01  WS-FMT-STR                  PIC X(11).                        STRFMT
       01  WS-I                        USAGE COMP-2.                     STRFMT
       01  WS-L                        USAGE COMP-2.                     STRFMT
       
       LINKAGE SECTION.
       01  LK-SRC-NUM                  USAGE COMP-2.
       01  LK-RES-STR                  PIC X(11).

       PROCEDURE DIVISION USING LK-SRC-NUM, LK-RES-STR.
           COPY STRFMT.
           GOBACK.
       