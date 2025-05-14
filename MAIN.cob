       IDENTIFICATION DIVISION.
       PROGRAM-ID. G4SSGCRE.
       AUTHOR.     Markku Sukanen.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. WIN11 WITH DEBUGGING MODE.
      *INPUT-OUTPUT SECTION.
      *FILE-CONTROL.
       DATA DIVISION.
      *FILE-SECTION.

       WORKING-STORAGE SECTION.
      *********
      * Random number generation:
      *********
      *01  RND-SEED-VALUE          PIC 9(4) COMP-5 VALUE 0.
       01  RND-INT                 PIC 9(5).
       01  WS-NUM-DICE             PIC S9(1) COMP-5.
      *********
      * Parsed run params:
      *********
       01  PARSED-PARM.
           05  PARM-LEN            PIC 9(3).
           05  PARSED-FIELD        PIC X(20).
           05  REMAINING-STRING    PIC X(100).
           05  PARM-INDEX          PIC 9(3) VALUE 1.
           05  CURRENT-PARM-NUM    PIC 9(4) VALUE 0.
      *********
      * Star system data goes here:
      *********    
       01  MASS-INDEX-OFFSET    PIC 99.
       01  STAR-INDEX           PIC 99.
       01  STAR-SYSTEM.
           05 IN-CLUSTER-OR-CORE   PIC X VALUE 'N'.
           05 STAR-SYSTEM-NAME     PIC x(48).
           05 NUM-OF-STARS         PIC 99.
           05 STAR OCCURS 1 TO 10 TIMES DEPENDING ON NUM-OF-STARS.
      * A-Z/##/###, or SPACE
               10 ORDERING             PIC X(3) VALUE SPACES.
      * Mass-index 0-33. Negative numbers are reserved for future use.
               10 MASS-INDEX           PIC 99.
               10 STAR-MASS            PIC 9V9(2).

       LINKAGE SECTION.
      *********
      * "Command-line" params and such go here.
      *********
       01  PARM.
           05  LK-PARM-LEN            PIC 9(4).
           05  LK-PARM-VAL            PIC X(100).
       
       PROCEDURE DIVISION USING PARM.
      *********
      * Parse PARM.
      *********
      *    DISPLAY 'LK-PARM-LEN: ' LK-PARM-LEN.
      *    DISPLAY 'LK-PARM-VAL: ' LK-PARM-VAL.
           COMPUTE PARM-LEN = FUNCTION LENGTH(
                                FUNCTION TRIM(LK-PARM-VAL)).
           PERFORM UNTIL PARM-INDEX > PARM-LEN
               INITIALIZE PARSED-FIELD
               UNSTRING LK-PARM-VAL
                   DELIMITED BY ","
                   INTO PARSED-FIELD
                   WITH POINTER PARM-INDEX
               END-UNSTRING

               IF PARSED-FIELD(1:1) = 'C' THEN
                   MOVE 'Y' TO IN-CLUSTER-OR-CORE
                   DISPLAY 'Generating system in a cluster/core.'
               END-IF

               ADD 1 TO CURRENT-PARM-NUM
      D        DISPLAY PARM-INDEX ' is ' CURRENT-PARM-NUM
      D                ': [' PARSED-FIELD ']'
           END-PERFORM.

           PERFORM DETERMINE-NUM-STARS.
           MOVE 1 TO STAR-INDEX.
           PERFORM DETERMINE-MASS-INDEX.
           PERFORM DETERMINE-MASS.

           GOBACK.

      *********
      * Determine number of stars.
      *********
       DETERMINE-NUM-STARS.
           CALL '3D6' USING RND-INT.
           EVALUATE TRUE
               WHEN RND-INT IS LESS OR EQUAL TO 10
                   MOVE 1 TO NUM-OF-STARS
               WHEN RND-INT IS LESS OR EQUAL TO 15
                   MOVE 2 TO NUM-OF-STARS
               WHEN OTHER
                   MOVE 3 TO NUM-OF-STARS
           END-EVALUATE
           DISPLAY 'NUM-OF-STARS: ' NUM-OF-STARS.
           GOBACK.

      *********
      * Determine mass index.
      *********
       DETERMINE-MASS-INDEX.
           CALL '3D6' USING RND-INT.
           EVALUATE TRUE
               WHEN RND-INT = 3
                   MOVE 0 TO MASS-INDEX-OFFSET
                   PERFORM DETERMINE-MASS-INDEX-2
               WHEN RND-INT = 4
                   MOVE 2 TO MASS-INDEX-OFFSET
                   PERFORM DETERMINE-MASS-INDEX-3
               WHEN RND-INT = 5
                   MOVE 5 TO MASS-INDEX-OFFSET
                   PERFORM DETERMINE-MASS-INDEX-4
               WHEN RND-INT = 6
                   MOVE 9 TO MASS-INDEX-OFFSET
                   PERFORM DETERMINE-MASS-INDEX-5
               WHEN RND-INT = 7
                   MOVE 14 TO MASS-INDEX-OFFSET
                   PERFORM DETERMINE-MASS-INDEX-5
               WHEN RND-INT = 8
                   MOVE 19 TO MASS-INDEX-OFFSET
                   PERFORM DETERMINE-MASS-INDEX-5
               WHEN RND-INT = 9
                   MOVE 24 TO MASS-INDEX-OFFSET
                   PERFORM DETERMINE-MASS-INDEX-3
               WHEN RND-INT = 10
                   MOVE 27 TO MASS-INDEX-OFFSET
                   PERFORM DETERMINE-MASS-INDEX-3
               WHEN RND-INT = 11
                   MOVE 30 TO MASS-INDEX(STAR-INDEX)
               WHEN RND-INT = 12
                   MOVE 31 TO MASS-INDEX(STAR-INDEX)
               WHEN RND-INT = 13
                   MOVE 32 TO MASS-INDEX(STAR-INDEX)
               WHEN OTHER
                   MOVE 33 TO MASS-INDEX(STAR-INDEX)
           END-EVALUATE
           GOBACK.

      *********
      * Determine mass index; X choices.
      *********
       DETERMINE-MASS-INDEX-2.
           CALL '3D6' USING RND-INT.
           IF RND-INT IS LESS OR EQUAL TO 10
               MOVE 0 TO MASS-INDEX(STAR-INDEX)
           ELSE
               MOVE 1 TO MASS-INDEX(STAR-INDEX)
           END-IF.
           GOBACK.

       DETERMINE-MASS-INDEX-3.
           CALL '3D6' USING RND-INT.
           EVALUATE TRUE
               WHEN RND-INT IS LESS OR EQUAL TO 8
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 0 + MASS-INDEX-OFFSET
               WHEN RND-INT IS LESS OR EQUAL TO 11
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 1 + MASS-INDEX-OFFSET
               WHEN OTHER
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 2 + MASS-INDEX-OFFSET
           END-EVALUATE.
           GOBACK.

       DETERMINE-MASS-INDEX-4.
           CALL '3D6' USING RND-INT.
           EVALUATE TRUE
               WHEN RND-INT IS LESS OR EQUAL TO 7
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 0 + MASS-INDEX-OFFSET
               WHEN RND-INT IS LESS OR EQUAL TO 10
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 1 + MASS-INDEX-OFFSET
               WHEN RND-INT IS LESS OR EQUAL TO 12
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 2 + MASS-INDEX-OFFSET
               WHEN OTHER
                   COMPUTE MASS-INDEX(STAR-INDEX)
                        = 3 + MASS-INDEX-OFFSET
           END-EVALUATE.
           GOBACK.

       DETERMINE-MASS-INDEX-5.
           CALL '3D6' USING RND-INT.
           EVALUATE TRUE
               WHEN RND-INT IS LESS OR EQUAL TO 7
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 0 + MASS-INDEX-OFFSET
               WHEN RND-INT IS LESS OR EQUAL TO 9
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 1 + MASS-INDEX-OFFSET
               WHEN RND-INT = 10
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 2 + MASS-INDEX-OFFSET
               WHEN RND-INT IS LESS OR EQUAL TO 12
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 3 + MASS-INDEX-OFFSET
               WHEN OTHER
                   COMPUTE MASS-INDEX(STAR-INDEX)
                       = 4 + MASS-INDEX-OFFSET
           END-EVALUATE.
           GOBACK.
      
      *********
      * Determine star mass (in solar masses) based on its mass-index.
      *********
       DETERMINE-MASS.
           EVALUATE TRUE
               WHEN MASS-INDEX(STAR-INDEX) = 0
                   MOVE 2.0 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 1
                   MOVE 1.9 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 2
                   MOVE 1.8 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 3
                   MOVE 1.7 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 4
                   MOVE 1.6 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 5
                   MOVE 1.5 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 6
                   MOVE 1.45 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 7
                   MOVE 1.4 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 8
                   MOVE 1.35 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 9
                   MOVE 1.3 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 10
                   MOVE 1.25 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 11
                   MOVE 1.2 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 12
                   MOVE 1.15 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 13
                   MOVE 1.1 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 14
                   MOVE 1.05 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 15
                   MOVE 1.0 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 16
                   MOVE 0.95 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 17
                   MOVE 0.9 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 18
                   MOVE 0.85 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 19
                   MOVE 0.8 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 20
                   MOVE 0.75 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 21
                   MOVE 0.7 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 22
                   MOVE 0.65 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 23
                   MOVE 0.6 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 24
                   MOVE 0.55 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 25
                   MOVE 0.5 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 26
                   MOVE 0.45 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 27
                   MOVE 0.4 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 28
                   MOVE 0.35 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 29
                   MOVE 0.3 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 30
                   MOVE 0.25 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 31
                   MOVE 0.2 TO STAR-MASS(STAR-INDEX)
               WHEN MASS-INDEX(STAR-INDEX) = 32
                   MOVE 0.15 TO STAR-MASS(STAR-INDEX)
               WHEN OTHER
                   MOVE 0.1 TO STAR-MASS(STAR-INDEX)
           END-EVALUATE.
           GOBACK.

      *********
      * Determine ordering based on star-index.
      *********
       DETERMINE-ORDERING.
           IF NUM-OF-STARS = 1 THEN
      *        Ordering is "meaningless" when there's only one star.
               INITIALIZE ORDERING(1)
               GOBACK
           END-IF.

           EVALUATE TRUE
               WHEN STAR-INDEX = 1 MOVE 'A' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 2 MOVE 'B' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 3 MOVE 'C' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 4 MOVE 'D' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 5 MOVE 'E' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 6 MOVE 'F' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 7 MOVE 'G' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 8 MOVE 'H' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 9 MOVE 'I' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 10 MOVE 'J' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 11 MOVE 'K' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 12 MOVE 'L' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 13 MOVE 'M' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 14 MOVE 'N' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 15 MOVE 'O' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 16 MOVE 'P' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 17 MOVE 'Q' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 18 MOVE 'R' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 19 MOVE 'S' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 20 MOVE 'T' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 21 MOVE 'U' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 22 MOVE 'V' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 23 MOVE 'W' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 24 MOVE 'X' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 25 MOVE 'Y' TO ORDERING(STAR-INDEX)
               WHEN STAR-INDEX = 26 MOVE 'Z' TO ORDERING(STAR-INDEX)
      D        WHEN OTHER MOVE '<U>' TO ORDERING(STAR-INDEX)
           END-EVALUATE.
           GOBACK.
