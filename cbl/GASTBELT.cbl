       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GEN-ASTEROID-BELT.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 5, 2025.
      ******************************************************************
      *
      * Generate an asteroid belt.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  D6                          PIC 99 USAGE COMP-3.
       01  WS-RND                      PIC 99 USAGE COMP-3.

       LINKAGE SECTION.
       01  LK-BELT.
           COPY EASTBELT.

       PROCEDURE DIVISION USING LK-BELT.
           COMPUTE WS-RND = FUNCTION RANDOM * 100.0
           EVALUATE TRUE
               WHEN WS-RND IS LESS OR EQUAL TO 75.0
                   SET BELT-C TO TRUE
                   GOBACK
           END-EVALUATE

           CALL '1D6' USING D6
           IF D6 IS LESS OR EQUAL TO 2
               SET BELT-M TO TRUE
           ELSE
               SET BELT-S TO TRUE
           END-IF
           GOBACK.
