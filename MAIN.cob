       IDENTIFICATION DIVISION.
       PROGRAM-ID. G4SSGCRE.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      * Star system generator, based on "GURPS 4e Space" rules.
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. WSL WITH DEBUGGING MODE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE ASSIGN TO "data/SPECS.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01  CSV-RECORD.
           05  CSV-LINE                PIC X(50).

       WORKING-STORAGE SECTION.
      *********
      * Random number generation:
      *********
      *01  RND-SEED-VALUE              PIC 9(4) COMP-5 VALUE 0.
       01  DICEROLL                    PIC 99 USAGE COMP-3.
       01  D6                          PIC 9(3) USAGE COMP-3.
       01  DR1                         PIC 9(5) VALUE 99.
       01  DR2                         PIC 9(5) VALUE 99.
       01  DR3                         PIC 9(5) VALUE 99.
       01  DR4                         PIC 9(5) VALUE 99.
       01  WS-IDX                      INDEX.
       01  TMP-NUM1                    PIC 9(5)V9(5) USAGE COMP-3.
       01  TMP-NUM2                    PIC 9(5)V9(5) USAGE COMP-3.
      D01  TMP-NUMD                    PIC 9(5)V9(5) VALUE 0.0.
       01  TEN-PERCENT                 PIC 9(5)V9(5) USAGE COMP-3
                                           VALUE 10.0.
       01  FIVE-PERCENT                PIC 9(5)V9(5) USAGE COMP-3
                                           VALUE  5.0.
      *********
      * Parsed run params:
      *********
       01  PARSED-PARM.
           05  PARM-LEN                PIC 9(3).
           05  PARSED-FIELD            PIC X(20).
           05  REMAINING-STRING        PIC X(100).
           05  PARM-INDEX              PIC 9(3) VALUE 1.
      *    05  CURRENT-PARM-NUM        PIC 9(4) VALUE 0.
      *********
      * Misc. variables/constants.
      *********
       77  MAX-EVO                     PIC 99 VALUE 34.                 CONSTANT
      *    200 as maximum number of stars is probably overkill, but who
      *    knows... there might be such systems in existence afterall
      *    somewhere in the universe.
       77  MAX-STARS                   PIC 999 VALUE 200.               CONSTANT
       77  INF-LIFESPAN                PIC 9(5)V9 VALUE 99999.9.        CONSTANT
       77  NOT-AVAILABLE               PIC 9V9 VALUE 0.0.               CONSTANT
      * A, L, M, S, T are used in a number of places for COMPUTE clarity.
       01  A                           PIC 9(5)V9(5) USAGE COMP-3.        TMP
       01  L                           PIC 9(5)V9(5) USAGE COMP-3.        TMP
       01  M                           PIC 9(5)V9(5) USAGE COMP-3.        TMP
       01  S                           PIC 9(5)V9(5) USAGE COMP-3.        TMP
       01  T                           PIC 9(5)V9(5) USAGE COMP-3.        TMP
       01  WS-NUMSTR                   PIC ZZZZZZZZZZZ.                 Z-STRING
      *********
      * Star system data goes here:
      *********
      *    Evolution info is read from a CSV file.
       01  WS-EVO-CSV.
           02  WS-CSV-MASS             PIC X(10).
           02  WS-CSV-APPROX-TYPE      PIC X(10).
           02  WS-CSV-AVG-TEMP         PIC X(10).
           02  WS-CSV-L-MIN            PIC X(10).
           02  WS-CSV-L-MAX            PIC X(10).
           02  WS-CSV-M-SPAN           PIC X(10).
           02  WS-CSV-S-SPAN           PIC X(10).
           02  WS-CSV-G-SPAN           PIC X(10).
      *    Parsed Evolution Info
       01  EVOLUTION OCCURS 34 TIMES                                    ^MAX-EVO
               INDEXED BY EVO-INDEX.
           02  EVO-MASS                PIC 9(5)V9(5) USAGE COMP-3.
           02  EVO-APPROX-TYPE         PIC X9.
           02  EVO-AVG-TEMP            PIC 9(5)V9(2) USAGE COMP-3.
           02  EVO-L-MIN               PIC 9(5)V9(5) USAGE COMP-3.
      *        L-Max of ZERO/- means there's no M/S/G spans and that
      *        the star's luminosity is determined by L-Min alone.
           02  EVO-L-MAX               PIC 9(5)V9(5) USAGE COMP-3.
      *        M-span of ZERO/- means that the star will remain main-
      *        sequence literally "forever". Lacking S/G-spans means
      *        that the star silently dwindles into D-class, which also
      *        happens if the star's age exceeds M+S+G-span total.
           02  EVO-M-SPAN              PIC 9(5)V9(5) USAGE COMP-3.
           02  EVO-S-SPAN              PIC 9(5)V9(5) USAGE COMP-3.
           02  EVO-G-SPAN              PIC 9(5)V9(5) USAGE COMP-3.
       01  MASS-IDX-OFF                PIC 99.
       01  CREATING-COMPANION          PIC X VALUE 'N'.
      *********
      * All the info pertaining to a star system that is/was being
      * generated:
      *********
       01  STAR-SYSTEM.
           05  IN-CLUSTER-OR-CORE      PIC X VALUE 'N'.
           05  STAR-SYSTEM-NAME        PIC X(48).
      *        Stellar populations, in order of general age:
      *            E1, Y1, I1, O1, I2, and E2.
           05  STELLAR-POPULATION      PIC XX.
      *        Stellar age is in BYR (billions of years).
           05  STELLAR-AGE             PIC 9(5)V9(5) USAGE COMP-3.
           05  NUM-OF-STARS            PIC 99.
      *        Individual star(s) of the system live here.
           05  STAR OCCURS 1 TO 200 TIMES
                    DEPENDING ON NUM-OF-STARS
                    INDEXED BY STAR-INDEX.
      *            Ordering is: A-Z/##/###, or SPACE
               10  ORDERING            PIC X(3) VALUE SPACES.
      *            Mass-index, refers to EVOLUTION.
               10  MASS-INDEX          INDEX.
      *            Star stage is one of: D, V, IV, III.
      * TODO:      There are of course others stages/sizes, but we're
      *            not generating those (yet).
               10  STAGE               PIC X(4).
               10  MASS                PIC 9(5)V9(5) USAGE COMP-3.      ×Sol
               10  TEMPERATURE         PIC 9(5)V9(2) USAGE COMP-3.      K
               10  LUMINOSITY          PIC 9(5)V9(5) USAGE COMP-3.      ×Sol
               10  RADIUS              PIC 9(5)V9(5) USAGE COMP-3.      AU
               10  ORBIT.
                   15  ECCENTRICITY    PIC 9V9(2) USAGE COMP-3.
                   15  SEPARATION      PIC XX.
                   15  DISTANCE.
                       20  MINR        PIC 9(5)V9(3) USAGE COMP-3.      AU
                       20  AVGR        PIC 9(5)V9(3) USAGE COMP-3.      AU
                       20  MAXR        PIC 9(5)V9(3) USAGE COMP-3.      AU
               10  ORBITAL-LIMITS.
                   15  INNER-LIMIT     PIC 9(5)V9(5) USAGE COMP-3.      AU
                   15  OUTER-LIMIT     PIC 9(5)V9(5) USAGE COMP-3.      AU
                   15  SNOW-LINE       PIC 9(5)V9(5) USAGE COMP-3.      AU


       LINKAGE SECTION.
      *********
      * "Command-line" params and such go here.
      *********
       01  PARM.
           05  LK-PARM-LEN             PIC 9(4).
           05  LK-PARM-VAL             PIC X(100).
       
       PROCEDURE DIVISION USING PARM.
      ******************************************************************
      **                               * Obviously, this is the very
      **   RUN MAIN PROGRAM.           * entry point of the whole app.
      **                               * Onwards! ;-)
      ******************************************************************
      * Parse PARM.
      *********
           COMPUTE PARM-LEN =
                   FUNCTION LENGTH(FUNCTION TRIM(LK-PARM-VAL)).
           PERFORM UNTIL PARM-INDEX > PARM-LEN
               INITIALIZE PARSED-FIELD
               UNSTRING LK-PARM-VAL
                   DELIMITED BY ","
                   INTO PARSED-FIELD
                   WITH POINTER PARM-INDEX
               END-UNSTRING

               IF PARSED-FIELD(1:1) = 'C' THEN
                   MOVE 'Y' TO IN-CLUSTER-OR-CORE
                   DISPLAY 'Generating star system in a cluster/core.'
               END-IF
           END-PERFORM.

      *    Read and parse evolution specs.csv, line by line.
           OPEN INPUT CSV-FILE.
      *D    DISPLAY '<CSV>'
           PERFORM VARYING EVO-INDEX
               FROM 1 BY 1 UNTIL EVO-INDEX > MAX-EVO
               READ CSV-FILE INTO CSV-LINE
                   NOT AT END
                       PERFORM PARSE-CSV
               END-READ
           END-PERFORM.
      *D    DISPLAY '</CSV>'
      *D    DISPLAY SPACE
           CLOSE CSV-FILE.

      *    Generate star system global specs.
           PERFORM DETERMINE-NUM-STARS.
           PERFORM DETERMINE-STELLAR-AGE.
      
      *    Generate star-specific data.
           PERFORM VARYING STAR-INDEX FROM 1 BY 1
                   UNTIL STAR-INDEX > NUM-OF-STARS
      D        DISPLAY SPACE
      D        DISPLAY 'Generating STAR-INDEX('STAR-INDEX')'
               PERFORM DETERMINE-ORDERING
               PERFORM DETERMINE-MASS-INDEX
               PERFORM DETERMINE-STELLAR-CHARACTERISTICS
               PERFORM COMPANION-STAR-ORBITS
               PERFORM DETERMINE-ORBIT-LIMITS
               PERFORM PLACE-PLANETS
           END-PERFORM.

      ******************************************************************  T
      *                                * "Bye bye cruel world for now!"    H
           EXIT PROGRAM.                                                 E  E
      *                                * "But see you soon again!"        N
      ******************************************************************   D

      *********
      * Parse evolution CSV.
      *********
       PARSE-CSV.
           UNSTRING CSV-LINE DELIMITED BY ","
                    INTO WS-CSV-MASS
                         WS-CSV-APPROX-TYPE
                         WS-CSV-AVG-TEMP
                         WS-CSV-L-MIN
                         WS-CSV-L-MAX
                         WS-CSV-M-SPAN
                         WS-CSV-S-SPAN
                         WS-CSV-G-SPAN
      *    Validate CSV entries...
           COMPUTE EVO-MASS(EVO-INDEX) ROUNDED =
               FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-MASS))
           MOVE FUNCTION TRIM(WS-CSV-APPROX-TYPE)
               TO EVO-APPROX-TYPE(EVO-INDEX)
           COMPUTE EVO-AVG-TEMP(EVO-INDEX) ROUNDED =
               FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-AVG-TEMP))
           COMPUTE EVO-L-MIN(EVO-INDEX) ROUNDED =
               FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-L-MIN))
           IF FUNCTION TRIM(WS-CSV-L-MAX) = "-" THEN
               MOVE 0.0 TO EVO-L-MAX(EVO-INDEX)
           ELSE
               COMPUTE EVO-L-MAX(EVO-INDEX) ROUNDED =
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-L-MAX))
           END-IF
      *    M-Span is special in that INF-LIFESPAN represents near
      *    infinite lifespan (as far as the universe is concerned).
           IF FUNCTION TRIM(WS-CSV-M-SPAN) = "-" THEN
               MOVE INF-LIFESPAN TO EVO-M-SPAN(EVO-INDEX)
           ELSE
               COMPUTE EVO-M-SPAN(EVO-INDEX) ROUNDED =
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-M-SPAN))
           END-IF
           IF FUNCTION TRIM(WS-CSV-S-SPAN) = "-" THEN
               MOVE 0.0 TO EVO-S-SPAN(EVO-INDEX)
           ELSE
               COMPUTE EVO-S-SPAN(EVO-INDEX) ROUNDED =
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-S-SPAN))
           END-IF
           IF FUNCTION TRIM(WS-CSV-G-SPAN) = "-" THEN
               MOVE 0.0 TO EVO-G-SPAN(EVO-INDEX)
           ELSE
               COMPUTE EVO-G-SPAN(EVO-INDEX) ROUNDED =
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-G-SPAN))
           END-IF
      *D    DISPLAY CSV-LINE
           EXIT.

      *********
      * Determine number of stars in the star system. There might be
      * more, but that is determined elsewhere/later.
      *********
       DETERMINE-NUM-STARS.
           CALL '3D6' USING DICEROLL.
           EVALUATE TRUE
               WHEN DICEROLL IS LESS OR EQUAL TO 10
                   MOVE 1 TO NUM-OF-STARS
               WHEN DICEROLL IS LESS OR EQUAL TO 15
                   MOVE 2 TO NUM-OF-STARS
               WHEN OTHER
                   MOVE 3 TO NUM-OF-STARS
           END-EVALUATE
      D    DISPLAY 'NUM-OF-STARS: ' NUM-OF-STARS.
           EXIT.

      *********
      * Determine mass index randomly. Set CREATING-COMPANION to 'Y'
      * if/when generating a (surprise/extra) companion star outside
      * predetermined count of stars.
      *********
       DETERMINE-MASS-INDEX.
           CALL '3D6' USING DICEROLL
           EVALUATE TRUE
               WHEN DICEROLL = 3
                   MOVE 2 TO A
               WHEN DICEROLL = 4
                   MOVE 3 TO MASS-IDX-OFF
                   MOVE 3 TO A
               WHEN DICEROLL = 5
                   MOVE 6 TO MASS-IDX-OFF
                   MOVE 4 TO A
               WHEN DICEROLL = 6
                   MOVE 10 TO MASS-IDX-OFF
                   MOVE 5 TO A
               WHEN DICEROLL = 7
                   MOVE 15 TO MASS-IDX-OFF
                   MOVE 5 TO A
               WHEN DICEROLL = 8
                   MOVE 20 TO MASS-IDX-OFF
                   MOVE 5 TO A
               WHEN DICEROLL = 9
                   MOVE 25 TO MASS-IDX-OFF
                   MOVE 3 TO A
               WHEN DICEROLL = 10
                   MOVE 28 TO MASS-IDX-OFF
                   MOVE 3 TO A
               WHEN DICEROLL = 11
                   MOVE 31 TO MASS-INDEX(STAR-INDEX)
               WHEN DICEROLL = 12
                   MOVE 32 TO MASS-INDEX(STAR-INDEX)
               WHEN DICEROLL = 13
                   MOVE 33 TO MASS-INDEX(STAR-INDEX)
               WHEN OTHER
                   MOVE 34 TO MASS-INDEX(STAR-INDEX)
           END-EVALUATE

           IF DICEROLL IS LESS THAN 11 THEN
      *        We do further randomizing only if the EVALUATE above
      *        didn't give us a singular/solid MASS-INDEX right away.
               PERFORM DETERMINE-MASS-INDEX-X
           END-IF

           IF CREATING-COMPANION = 'Y' THEN
      *        If creating a companion star, we step mass-index deeper
      *        by 1d6-1 steps (capping to MAX-EVO, of course).
               CALL '1D6' USING DICEROLL
               COMPUTE D6 = MASS-INDEX(STAR-INDEX) + DICEROLL - 1
               IF D6 > MAX-EVO THEN
                   MOVE MAX-EVO TO D6
               END-IF
               MOVE D6 TO MASS-INDEX(STAR-INDEX)
               MOVE 'N' TO CREATING-COMPANION
           END-IF
      D    DISPLAY 'MIDX 'MASS-INDEX(STAR-INDEX)
           EXIT.

      *********
      * Determine mass index; X choices each variant.
      *
      * Upon call:
      *    A            = number of choices.
      *    MASS-IDX-OFF = initial MASS-INDEX offset.
      *********
       DETERMINE-MASS-INDEX-X.
           CALL '3D6' USING DICEROLL.
           IF A = 2 THEN
               IF DICEROLL IS LESS OR EQUAL TO 10
                   MOVE 1 TO MASS-INDEX(STAR-INDEX)
               ELSE
                   MOVE 2 TO MASS-INDEX(STAR-INDEX)
               END-IF
           ELSE
               INITIALIZE DR3
               INITIALIZE DR4
               EVALUATE TRUE
                   WHEN A = 3
                       MOVE 8 TO DR1
                       MOVE 11 TO DR2
                   WHEN A = 4
                       MOVE 7 TO DR1
                       MOVE 10 TO DR2
                       MOVE 12 TO DR3
                   WHEN OTHER
                       MOVE 7 TO DR1
                       MOVE 9 TO DR2
                       MOVE 10 TO DR3
                       MOVE 12 TO DR4
               END-EVALUATE
              
               MOVE MASS-IDX-OFF TO WS-IDX
               EVALUATE TRUE
                   WHEN DICEROLL IS LESS OR EQUAL TO DR1
      *                COMPUTE WS-IDX = WS-IDX + 0                      NOP
                   WHEN DICEROLL IS LESS OR EQUAL TO DR2
                       COMPUTE WS-IDX = WS-IDX + 1
                   WHEN DICEROLL IS LESS OR EQUAL TO DR3
                       COMPUTE WS-IDX = WS-IDX + 2
                   WHEN DICEROLL IS LESS OR EQUAL TO DR4
                       COMPUTE WS-IDX = WS-IDX + 3
                   WHEN OTHER
                       COMPUTE WS-IDX = WS-IDX + 4
               END-EVALUATE
               MOVE WS-IDX TO MASS-INDEX(STAR-INDEX)
           END-IF
           EXIT.

      *********
      * Determine various stellar characteristics.
      *********
       DETERMINE-STELLAR-CHARACTERISTICS.
           PERFORM SET-STAR-STAGE
           PERFORM DETERMINE-STELLAR-MASS
           PERFORM COMPUTE-LUMINOSITY
           PERFORM DETERMINE-TEMPERATURE
           PERFORM COMPUTE-STAR-RADIUS
           EXIT.

      *********
      * Determine star mass based on its mass-index.
      *********
       DETERMINE-STELLAR-MASS.
           IF STAGE(STAR-INDEX) = 'D' THEN
               CALL '2D6' USING D6
               MOVE 0.05 TO TMP-NUM1
               MOVE 0.9 TO TMP-NUM2
               COMPUTE MASS(STAR-INDEX) =
                       D6 * TMP-NUM1 + TMP-NUM2
           ELSE
               MOVE EVO-MASS(MASS-INDEX(STAR-INDEX))
                    TO MASS(STAR-INDEX)
           END-IF
      D    MOVE STAR-INDEX TO WS-NUMSTR
      D    DISPLAY 'MASS 'MASS(STAR-INDEX)' × Sol'
           EXIT.

      *********
      * Determine star ordering in the system based on star-index(es).
      *********
       DETERMINE-ORDERING.
           IF NUM-OF-STARS = 1 THEN
      *        Ordering is "meaningless" when there's only one star.
               INITIALIZE ORDERING(1)
               EXIT
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
           EXIT.

      *********
      * Determine stellar age and population of the star system.
      *********
       DETERMINE-STELLAR-AGE.
           CALL '3D6' USING DICEROLL.
           IF DICEROLL = 3 THEN
               MOVE 'E1' TO STELLAR-POPULATION
               MOVE 0.0 TO STELLAR-AGE
           ELSE
               CALL '3D6' USING DICEROLL
               CALL '1D6' USING D6
               IF DICEROLL IS LESS OR EQUAL TO 6
                   MOVE 'Y1' TO STELLAR-POPULATION
                   CALL '1D6' USING DICEROLL
                   COMPUTE STELLAR-AGE
                           = (DICEROLL - 1) * 0.3
                           + (D6 - 1) * 0.05
                           + 0.1
               ELSE
                   CALL '1D6' USING DICEROLL
                   COMPUTE STELLAR-AGE
                           = (DICEROLL - 1) * 0.6
                           + (D6 - 1) * 0.1
                   EVALUATE TRUE
                       WHEN DICEROLL IS LESS OR EQUAL TO 10
                           MOVE 'I1' TO STELLAR-POPULATION
                           COMPUTE STELLAR-AGE = STELLAR-AGE + 2.0
                       WHEN DICEROLL IS LESS OR EQUAL TO 14
                           MOVE 'O1' TO STELLAR-POPULATION
                           COMPUTE STELLAR-AGE = STELLAR-AGE + 5.6
                       WHEN DICEROLL IS LESS OR EQUAL TO 17
                           MOVE 'I2' TO STELLAR-POPULATION
                           COMPUTE STELLAR-AGE = STELLAR-AGE + 8.0
                       WHEN OTHER
                           MOVE 'E2' TO STELLAR-POPULATION
                           COMPUTE STELLAR-AGE = STELLAR-AGE + 10.0
                   END-EVALUATE
               END-IF
           END-IF
      D    DISPLAY 'POP  'STELLAR-POPULATION
      D    DISPLAY 'AGE  'STELLAR-AGE' BYr'
           EXIT.

      *********
      * Determine star's temperature in kelvins based on mass-index.
      *********
       DETERMINE-TEMPERATURE.
           IF STAGE(STAR-INDEX) = 'D' THEN
      * TODO:  figure out proper White Dwarf surface temperature instead
      *        of using this placeholder value of 10000.0.
               MOVE 10000.0 TO TEMPERATURE(STAR-INDEX)
           ELSE
               MOVE EVO-AVG-TEMP(MASS-INDEX(STAR-INDEX))
                    TO TEMPERATURE(STAR-INDEX)
               EVALUATE TRUE
                   WHEN STAGE(STAR-INDEX) = 'IV'
      *                T = M - (( A / S ) * ( M - 4800 ))
                       MOVE TEMPERATURE(STAR-INDEX) TO M
                       MOVE STELLAR-AGE TO A
                       COMPUTE A
                             = A - EVO-M-SPAN(MASS-INDEX(STAR-INDEX))
                       MOVE EVO-S-SPAN(MASS-INDEX(STAR-INDEX)) TO S
                       COMPUTE TEMPERATURE(STAR-INDEX) ROUNDED
                             = M - ((A / S) * (M - 4800.0))
                   WHEN STAGE(STAR-INDEX) = 'III' OR
                        STAGE(STAR-INDEX) = 'II'  OR
                        STAGE(STAR-INDEX) = 'Ib'  OR
                        STAGE(STAR-INDEX) = 'Ia'
                       COMPUTE TEMPERATURE(STAR-INDEX) ROUNDED
                             = 3000.0 + (2000.0 * FUNCTION RANDOM)
               END-EVALUATE
      *        +/- 100K
               MOVE 100.0 TO TMP-NUM1
      *        Note: we need to route temperature via temporary variable
      *              because of data format difference (V9(5) vs V9(2)).
               CALL 'ALTER-VALUE-BY-UPTO'
                   USING TMP-NUM1,
                         TEMPERATURE(STAR-INDEX),
                         TMP-NUM2
               COMPUTE TEMPERATURE(STAR-INDEX) ROUNDED = TMP-NUM2
           END-IF
      D    DISPLAY 'TEMP 'TEMPERATURE(STAR-INDEX)'K'
           EXIT.

      *********
      * Determine star's stage based on its mass-index and stellar age.
      *********
       SET-STAR-STAGE.
           IF EVO-M-SPAN(MASS-INDEX(STAR-INDEX)) >= STELLAR-AGE
               MOVE 'V' TO STAGE(STAR-INDEX)
           ELSE
               IF EVO-S-SPAN(MASS-INDEX(STAR-INDEX)) = 0.0
                   MOVE 'D' TO STAGE(STAR-INDEX)
               ELSE IF EVO-M-SPAN(MASS-INDEX(STAR-INDEX)) +
                       EVO-S-SPAN(MASS-INDEX(STAR-INDEX))
                       >= STELLAR-AGE
                       MOVE 'IV' TO STAGE(STAR-INDEX)
                    ELSE IF EVO-M-SPAN(MASS-INDEX(STAR-INDEX)) +
                            EVO-S-SPAN(MASS-INDEX(STAR-INDEX)) +
                            EVO-G-SPAN(MASS-INDEX(STAR-INDEX))
                            >= STELLAR-AGE
                             MOVE 'III' TO STAGE(STAR-INDEX)
                         ELSE
                             MOVE 'D' TO STAGE(STAR-INDEX)
                         END-IF
                    END-IF
               END-IF
           END-IF
      D    DISPLAY 'STGE 'STAGE(STAR-INDEX)
           EXIT.

      *********
      * Determine a star's luminosity based on its stage and other
      * in factoring things.
      *********
       COMPUTE-LUMINOSITY.
           EVALUATE TRUE
               WHEN STAGE(STAR-INDEX) = 'V'
                   IF EVO-L-MAX(MASS-INDEX(STAR-INDEX)) = NOT-AVAILABLE
                       MOVE EVO-L-MIN(MASS-INDEX(STAR-INDEX))
                           TO LUMINOSITY(STAR-INDEX)
                   ELSE
      *                L = MIN + (( A / S ) × ( MAX - MIN ))
                       COMPUTE LUMINOSITY(STAR-INDEX)
                           = EVO-L-MIN(MASS-INDEX(STAR-INDEX)) +
                           ((STELLAR-AGE /
                             EVO-M-SPAN(MASS-INDEX(STAR-INDEX))) *
                            (EVO-L-MAX(MASS-INDEX(STAR-INDEX)) -
                             EVO-L-MIN(MASS-INDEX(STAR-INDEX))))
                   END-IF
               WHEN STAGE(STAR-INDEX) = 'IV'
                   MOVE EVO-L-MAX(MASS-INDEX(STAR-INDEX))
                       TO LUMINOSITY(STAR-INDEX)
               WHEN STAGE(STAR-INDEX) = 'D'
                   MOVE 0.001 TO LUMINOSITY(STAR-INDEX)
               WHEN OTHER
      *            Generally III - Ia here ...
                   MOVE 25.0 TO TMP-NUM1
                   COMPUTE LUMINOSITY(STAR-INDEX) =
                       EVO-L-MAX(MASS-INDEX(STAR-INDEX)) * TMP-NUM1
           END-EVALUATE
           CALL 'ALTER-VALUE-BY-PERCENTAGE'
               USING TEN-PERCENT,
                     LUMINOSITY(STAR-INDEX),
                     TMP-NUM1
           COMPUTE LUMINOSITY(STAR-INDEX) = TMP-NUM1
      D    DISPLAY 'LUM  'LUMINOSITY(STAR-INDEX)' × Sol'
           EXIT.

      *********
      * Determine star's radius (in AU).
      *********
       COMPUTE-STAR-RADIUS.
           IF STAGE(STAR-INDEX) = 'D' THEN
               MOVE 0.0 TO RADIUS(STAR-INDEX)
           ELSE
      *        R = (155,000 × √L) / T²
               MOVE TEMPERATURE(STAR-INDEX) TO T
               COMPUTE L ROUNDED
                       = FUNCTION SQRT(LUMINOSITY(STAR-INDEX))
                       * 155000.0
               COMPUTE RADIUS(STAR-INDEX) ROUNDED = L / (T * T)
           END-IF
      D    DISPLAY 'RAD  'RADIUS(STAR-INDEX)' AU'
           EXIT.

      *********
      * Determine orbital eccentricity alongside min-avg-max orbit
      * distances in relation to parent (if exists).
      *
      * NOTE: STAR-INDEX(1) obviously has no parent ;-)
      *********
       COMPANION-STAR-ORBITS.
           CALL 'STAR-ORBITAL-DETAILS' USING
               NUM-OF-STARS,
               STAR-INDEX,
               ORBIT(STAR-INDEX).
      D    IF STAR-INDEX > 1 THEN
      D        DISPLAY 'OECC '
                       ECCENTRICITY(STAR-INDEX)
      D        DISPLAY '  min: 'MINR(STAR-INDEX)' AU'
      D        DISPLAY '  avg: 'AVGR(STAR-INDEX)' AU'
      D        DISPLAY '  max: 'MAXR(STAR-INDEX)' AU'
      D    END-IF
           EXIT.

      *********
      * Determine inner and outer planetary orbit limits and so called
      * "snow line".
      *
      * TODO: Forbidden Zones
      *
      *********
       DETERMINE-ORBIT-LIMITS.
           CALL 'STAR-ORBIT-LIMITS' USING
                   MASS(STAR-INDEX),
                   LUMINOSITY(STAR-INDEX),
                   ORBITAL-LIMITS(STAR-INDEX)
           EXIT.

      *********
      * Place planets!
      *
      * Start with gas giants, if any...
      *********
       PLACE-PLANETS.
           EXIT.
