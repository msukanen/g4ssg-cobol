       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORBITAL-LIMITS-TEST.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Test orbital limits generation.
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. WSL WITH DEBUGGING MODE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MASS                        PIC 9(5)V9(5) USAGE COMP-3.
       01  LUMINOSITY                  PIC 9(8)V9(2) USAGE COMP-3
                                           VALUE 1.0.
       01  ORBITAL-LIMITS.
           05  INNER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3.         AU
           05  OUTER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3.         AU
           05  SNOW-LINE               PIC 9(5)V9(5) USAGE COMP-3.         AU

       PROCEDURE DIVISION.
           PERFORM VARYING MASS
                   FROM 0.05 BY 0.05
                   UNTIL MASS > 2.05
               COMPUTE LUMINOSITY ROUNDED
                     = MASS ** (MASS ** (MASS ** MASS))
      *        PERFORM VARYING LUMINOSITY
      *                FROM 0.0001 BY 0.00005
      *                UNTIL LUMINOSITY > 100.0
                   CALL 'STAR-ORBIT-LIMITS'
                       USING MASS,
                             LUMINOSITY,
                             ORBITAL-LIMITS
      D            DISPLAY 'STAR-ORBIT-LIMITS:'
      D            DISPLAY '  mass      : 'MASS' x Sol'
      D            DISPLAY '  luminosity: 'LUMINOSITY' × Sol'
      D            DISPLAY '  inner     : 'INNER-LIMIT' AU'
      D            DISPLAY '  outer     : 'OUTER-LIMIT' AU'
      D            DISPLAY '  snow-line : 'SNOW-LINE' AU'
      *        END-PERFORM
           END-PERFORM.
           STOP RUN.
