       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOCATE-ORBITAL-ZONES.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Locate star's orbital zones/limits.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-I1                       USAGE COMP-2.
       01  WS-I2                       USAGE COMP-2.
       01  WS-L                        USAGE COMP-2.
       
       LINKAGE SECTION.
       01  LK-STAR.
           COPY STAR.
       01  LK-ORB-ZONES.
           COPY ORBZONE.

       PROCEDURE DIVISION USING LK-STAR, LK-ORB-ZONES.
           COMPUTE WS-L = LUMINOSITY
           COMPUTE WS-L = FUNCTION SQRT(WS-L)

      * Inner limit first:
           COMPUTE WS-I1 = 0.1 * MASS
           COMPUTE WS-I2 = 0.01 * WS-L
           IF WS-I1 > WS-I2 THEN
               COMPUTE OZ-INNER-LIMIT OF LK-ORB-ZONES ROUNDED = WS-I1
           ELSE
               COMPUTE OZ-INNER-LIMIT OF LK-ORB-ZONES ROUNDED = WS-I2
           END-IF

           COMPUTE OZ-OUTER-LIMIT OF LK-ORB-ZONES = 40.0 * MASS
           COMPUTE OZ-SNOW-LINE OF LK-ORB-ZONES = 4.85 * WS-L
           GOBACK.
