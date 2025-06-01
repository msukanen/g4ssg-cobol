       IDENTIFICATION DIVISION.
       PROGRAM-ID. DETERMINE-ORBITAL-ECCENTRICITY.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Determine orbital eccentricity. Eccentricity depends on orbital
      * separation to some degree.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY RNG.
       
       LINKAGE SECTION.
       01  LK-ORB-SEP-CATEGORY         PIC 9.
           COPY ORBSEP.
       01  ECCENTRICITY                PIC 9V99 USAGE COMP-3.

       PROCEDURE DIVISION USING LK-ORB-SEP-CATEGORY, ECCENTRICITY.
           CALL '3D6' USING D6
           EVALUATE TRUE
               WHEN SEP-VERY-CLOSE
                   COMPUTE D6 = D6 - 6
               WHEN SEP-CLOSE
                   COMPUTE D6 = D6 - 4
               WHEN SEP-MODERATE
                   COMPUTE D6 = D6 - 2
           END-EVALUATE

           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 3
                   MOVE 0 TO ECCENTRICITY
               WHEN D6 = 4
                   MOVE 0.1 TO ECCENTRICITY
               WHEN D6 = 5
                   MOVE 0.2 TO ECCENTRICITY
               WHEN D6 = 6
                   MOVE 0.3 TO ECCENTRICITY
               WHEN D6 IS LESS OR EQUAL TO 8
                   MOVE 0.4 TO ECCENTRICITY
               WHEN D6 IS LESS OR EQUAL TO 11
                   MOVE 0.5 TO ECCENTRICITY
               WHEN D6 IS LESS OR EQUAL TO 13
                   MOVE 0.6 TO ECCENTRICITY
               WHEN D6 IS LESS OR EQUAL TO 15
                   MOVE 0.7 TO ECCENTRICITY
               WHEN D6 = 16
                   MOVE 0.8 TO ECCENTRICITY
               WHEN D6 = 17
                   MOVE 0.9 TO ECCENTRICITY
               WHEN OTHER
                   MOVE 0.95 TO ECCENTRICITY
           END-EVALUATE
           GOBACK.
