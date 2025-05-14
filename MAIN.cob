       IDENTIFICATION DIVISION.
       PROGRAM-ID. G4SSGCRE.
       AUTHOR.     Markku Sukanen.
       
       ENVIRONMENT DIVISION.
      *INPUT-OUTPUT SECTION.
      *FILE-CONTROL.
       DATA DIVISION.
      *FILE-SECTION.

       WORKING-STORAGE SECTION.
      * Random number generations:
      *01  RND-SEED-VALUE          PIC 9(4) COMP-5 VALUE 0.
       01  RND-INT                 PIC 9(5).
       01  WS-NUM-DICE             PIC S9(1) COMP-5.
      * Parsed parameters:
       01  PARSED-PARM.
           05  PARM-LEN            PIC 9(3).
           05  PARSED-FIELD        PIC X(20).
           05  REMAINING-STRING    PIC X(100).
           05  PARM-INDEX          PIC 9(3) VALUE 1.
           05  WS-PARM-NUM         PIC 9(4) VALUE 0.
       01  STAR-SYSTEM.
           05 STAR-SYSTEM-NAME     PIC x(48).
           05 NUM-OF-STARS         PIC 9(2).

       LINKAGE SECTION.
      * "Command-line" params and such go here.
       01  PARM.
           05  LK-PARM-LEN            PIC 9(4).
           05  LK-PARM-VAL            PIC X(100).
       
       PROCEDURE DIVISION USING PARM.
      *****
      * Parse PARM.
      *****
      D    DISPLAY 'LK-PARM-LEN: ' LK-PARM-LEN.
      D    DISPLAY 'LK-PARM-VAL: ' LK-PARM-VAL.
           COMPUTE PARM-LEN = FUNCTION LENGTH(
                                FUNCTION TRIM(LK-PARM-VAL)).
           PERFORM UNTIL PARM-INDEX > PARM-LEN
               INITIALIZE PARSED-FIELD
               UNSTRING LK-PARM-VAL
                   DELIMITED BY ","
                   INTO PARSED-FIELD
                   WITH POINTER PARM-INDEX
               END-UNSTRING

               ADD 1 TO WS-PARM-NUM

      D        DISPLAY PARM-INDEX ' is ' WS-PARM-NUM
      D                ': [' PARSED-FIELD ']'
           END-PERFORM.

      *****
      * Determine number of stars.
      *****
           CALL 'DICENXD6' USING 3, RND-INT.
           IF RND-INT IS LESS OR EQUAL TO 10
               MOVE 1 TO NUM-OF-STARS
           ELSE IF RND-INT IS LESS OR EQUAL TO 15
               MOVE 2 TO NUM-OF-STARS
           ELSE
               MOVE 3 TO NUM-OF-STARS
           END-IF
      D    DISPLAY 'NUM-OF-STARS: ' NUM-OF-STARS.

           GOBACK.
