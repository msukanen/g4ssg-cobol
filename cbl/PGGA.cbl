       IDENTIFICATION DIVISION.
       PROGRAM-ID. DETERMINE-GAS-GIANT-ARRANGEMENT.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Determine gas giant arrangement, if any such happens to be in
      * the star system.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY RNG.

       LINKAGE SECTION.
       01  LK-GGA                      PIC 9.
           COPY GGA.

       PROCEDURE DIVISION USING LK-GGA.
           CALL '3D6' USING D6
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 10
                   SET NO-GAS-GIANT TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 12
                   SET CONVENTIONAL-GAS-GIANT TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 14
                   SET ECCENTRIC-GAS-GIANT TO TRUE
               WHEN OTHER
                   SET EPISTELLAR-GAS-GIANT TO TRUE
           END-EVALUATE
           GOBACK.
