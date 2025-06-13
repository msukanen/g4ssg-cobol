       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-ORBITAL-ECCENTRICITY.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 12, 2025.
      ******************************************************************
      *
      * Generate random orbital eccentricity value.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY    RNG.
       LINKAGE SECTION.
       01  LK-ORBIT-ECCENTRICITY       USAGE COMP-2.

       PROCEDURE DIVISION USING LK-ORBIT-ECCENTRICITY.
           COPY 3D6.
           EVALUATE TRUE
               WHEN D6 <=  3 MOVE 0.0 TO LK-ORBIT-ECCENTRICITY
               WHEN D6  =  4 MOVE 0.1 TO LK-ORBIT-ECCENTRICITY
               WHEN D6  =  5 MOVE 0.2 TO LK-ORBIT-ECCENTRICITY
               WHEN D6  =  6 MOVE 0.3 TO LK-ORBIT-ECCENTRICITY
               WHEN D6 <=  8 MOVE 0.4 TO LK-ORBIT-ECCENTRICITY
               WHEN D6 <= 11 MOVE 0.5 TO LK-ORBIT-ECCENTRICITY
               WHEN D6 <= 13 MOVE 0.6 TO LK-ORBIT-ECCENTRICITY
               WHEN D6 <= 15 MOVE 0.7 TO LK-ORBIT-ECCENTRICITY
               WHEN D6  = 16 MOVE 0.8 TO LK-ORBIT-ECCENTRICITY
               WHEN D6  = 17 MOVE 0.9 TO LK-ORBIT-ECCENTRICITY
               WHEN OTHER   MOVE 0.95 TO LK-ORBIT-ECCENTRICITY
           END-EVALUATE.
           GOBACK.
