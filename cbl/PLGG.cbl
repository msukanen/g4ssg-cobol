       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAS-GIANT-ARRANGEMENT.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Determine arrangement (or lack of) gas giant(s) in the system.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  D6                          PIC 99 USAGE COMP-3.

       LINKAGE SECTION.
       01  ORBIT-LIMITS.
           05  INNER-LIMIT             PIC 9(5)V9(5) USAGE COMP-3.
           05  IGN                     PIC 9(5)V9(5) USAGE COMP-3.
           05  SNOW-LINE               PIC 9(5)V9(5) USAGE COMP-3.
       01  GG-ARRANGEMENT.
           05  RAD                     PIC 9(5)V9(5) USAGE COMP-3.
           05  LK-ARRANGEMENT          PIC XX.
               88  NO-GAS-GIANT        VALUE 'NO'.
               88  CONVENTIONAL-GG     VALUE 'CO'.
               88  ECCENTRIC-GG        VALUE 'EC'.
               88  EPISTELLAR-GG       VALUE 'ES'.

       PROCEDURE DIVISION USING ORBIT-LIMITS, GG-ARRANGEMENT.
           CALL '3D6' USING D6
           EVALUATE TRUE
      *        Lets see if there is any gas giants present to begin with
      *        and figure out their orbit (if any present).
               WHEN D6 IS LESS OR EQUAL TO 10
                   SET NO-GAS-GIANT TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 12
                   SET CONVENTIONAL-GG TO TRUE
                   CALL '2D6' USING D6
                   COMPUTE D6 = D6 - 2
                   COMPUTE RAD ROUNDED = SNOW-LINE * (1.0 + (0.05 * D6))
               WHEN D6 IS LESS OR EQUAL TO 14
                   SET ECCENTRIC-GG TO TRUE
                   CALL '1D6' USING D6
                   COMPUTE RAD ROUNDED = SNOW-LINE * (0.125 * D6)
               WHEN OTHER
                   SET EPISTELLAR-GG to TRUE
                   CALL '3D6' USING D6
                   COMPUTE RAD ROUNDED = INNER-LIMIT * (0.1 * D6)
           END-EVALUATE
           GOBACK.
