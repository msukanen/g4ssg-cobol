       IDENTIFICATION DIVISION.
       PROGRAM-ID. G4SSG.
       AUTHOR.     Markku Sukanen.
       
       ENVIRONMENT DIVISION.
      *INPUT-OUTPUT SECTION.
      *FILE-CONTROL.
       DATA DIVISION.
      *FILE-SECTION.
       WORKING-STORAGE SECTION.
       01  RND-SEED_VALUE          PIC S9(4) COMP-5 VALUE 0.
       01  RND-NUM                 USAGE is COMP-1.
       01  RND-INT                 PIC 9(5).
       01  STAR-SYSTEM.
           05 STAR-SYSTEM-NAME     PIC x(48).
           05 NUM-OF-STARS         PIC 9(2).
       01  WS-NUM-DICE             PIC S9(1) COMP-5.
       
       PROCEDURE DIVISION.
           MOVE 3 to WS-NUM-DICE.
           CALL 'DICER000' using RND-SEED_VALUE, WS-NUM-DICE, RND-NUM.
           DISPLAY 'Random number (0 to 1): ' RND-NUM.
           
      *> Example: get a random integer between 1 and 100
           MOVE 1 to WS-NUM-DICE.
           CALL 'DICER000' using RND-SEED_VALUE, WS-NUM-DICE, RND-NUM.
           COMPUTE RND-INT = (RND-NUM * 100) + 1.
           DISPLAY 'Random int (1 to 100): ' RND-INT.

           GOBACK.

       