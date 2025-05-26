       IDENTIFICATION DIVISION.
       PROGRAM-ID. ORBITAL-LIMITS-TEST.
       AUTHOR.     Markku Sukanen.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. WSL WITH DEBUGGING MODE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  MASS                        PIC 9(5)V9(5) USAGE COMP-3.
       01  LUMINOSITY                  PIC 9(5)V9(5) USAGE COMP-3.
       01  ORBITAL-LIMITS.
           05  INNER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3.         AU
           05  OUTER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3.         AU
           05  SNOW-LINE               PIC 9(5)V9(5) USAGE COMP-3.         AU

       PROCEDURE DIVISION.
           MOVE 1.2 TO MASS.
           MOVE 0.03795 TO LUMINOSITY.
           CALL 'STAR-ORBIT-LIMITS' USING
                   MASS, LUMINOSITY, ORBITAL-LIMITS.
           STOP RUN.
