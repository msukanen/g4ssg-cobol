       IDENTIFICATION DIVISION.
       PROGRAM-ID.   G4SSGCRE.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. May 25 — June 9, 2025.
      ******************************************************************
      *
      * Star system generator, based on "GURPS 4e Space" rules.
      *
      ******************************************************************
       COPY        TESTENV.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CSV-FILE             ASSIGN TO "data/SPECS.csv"
                                       ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CSV-FILE.
       01  CSV-RECORD.
           05  CSV-LINE                PIC X(50).

       WORKING-STORAGE SECTION.
      *********************************
      * Random number generation:
       COPY RNG.                                                        D6 etc.
       01  WS-TMP-STR                  PIC X(100).                      FMT-NUM
       01  WS-TMP-N0                   USAGE COMP-2.
       01  WS-TMP-N1                   USAGE COMP-2.
       01  WS-TMP-N2                   USAGE COMP-2.
       01  WS-DELTA                    USAGE COMP-2.                    helper
       01  WS-RATIO                    USAGE COMP-2.                    helper
       01  WS-COUNT                    PIC 999 USAGE COMP-5.            helper
       01  WS-GG                       PIC X.                           helper
           88  IS-GG                   VALUE 'Y'
                                       WHEN SET TO FALSE IS 'N'.
       01  WS-DISTANCE                 USAGE COMP-2.                    helper
       01  WS-SNOW-LINE                USAGE COMP-2.                    helper
       01  WS-ADJACENT-INNER-OUTER     PIC X.
           88  IS-ADJACENT             VALUE 'Y'
                                       WHEN SET TO FALSE IS 'N'.
      *********************************
      * Parsed run params:
       01  PARSED-PARM.
           05  PARM-LEN                PIC 999 USAGE COMP-3.
           05  PARSED-FIELD            PIC X(20).
           05  PARM-INDEX              PIC 9 VALUE 1.
       01  WS-VERBOSITY                PIC X VALUE '-'.
           88  VERBOSE-OUTPUT          VALUE 'Y'
                                       WHEN SET TO FALSE IS '-'.
      *********************************
      * Misc. variables/constants.
       01  WS-WAS-CSV-COMMENT          PIC X VALUE 'N'.
           88  WAS-CSV-COMMENT         VALUE 'Y'
                                       WHEN SET TO FALSE IS 'N'.
       01  WS-WAS-CSV-ERROR            PIC X VALUE 'N'.
           88  WAS-CSV-ERROR           VALUE 'Y'
                                       WHEN SET TO FALSE IS 'N'.
       01  WS-FMT-DIGITS               PIC 9 VALUE 5.
       COPY CONST.
      * ... 200 as maximum number of stars is probably overkill ...
       77  MAX-STARS                   PIC 999 VALUE 200.               CONSTANT
       77  MAX-ORBITS                  PIC 999 VALUE 200.               CONSTANT
      * System generation basics.
       01  WS-IN-CLUSTER-OR-CORE       PIC X VALUE 'N'.
           88  IN-CLUSTER-OR-CORE      VALUE 'Y'
                                       WHEN SET TO FALSE IS 'N'.
       01  WS-THIRD-IN-TRINARY         PIC X VALUE 'N'.
           88  IS-THIRD-IN-TRINARY     VALUE 'Y'
                                       WHEN SET TO FALSE IS 'N'.
      *********************************
      * Stellar CSV related:
       01  WS-EVO-CSV.
           05  WS-CSV-MASS             PIC X(10).
           05  WS-CSV-APPROX-TYPE      PIC X(10).
           05  WS-CSV-AVG-TEMP         PIC X(10).
           05  WS-CSV-L-MIN            PIC X(10).
           05  WS-CSV-L-MAX            PIC X(10).
           05  WS-CSV-M-SPAN           PIC X(10).
           05  WS-CSV-S-SPAN           PIC X(10).
           05  WS-CSV-G-SPAN           PIC X(10).
       01  WS-EVO-REC.
           05  EVO-COUNT               PIC 999 COMP-5 VALUE 0.          max 100?
           05  STELLAR-EVO             OCCURS 0 TO 100 TIMES            ^MAXIMUM
                                       DEPENDING ON EVO-COUNT
                                       INDEXED BY EVO-IDX.
               COPY STLREVO.
      *********************************
      * Stellar data:
      *
       01  WS-STAR-SYSTEM.
           05  SYSTEM-AGE.
               COPY STLRAGE.cpy.
           05  STAR-COUNT              PIC 999 USAGE COMP-5 VALUE 0.    max 200?
           05  STAR                    OCCURS 0 TO 200 TIMES            ^MAXIMUM
                                       DEPENDING ON STAR-COUNT
                                       INDEXED BY
                                           STAR-IDX, PARENT-STAR-IDX.
               COPY STARDATA.
      *    Note that separation data ~should~ be ignored for the fiINFO!
      *    star in the system, entirely — it has meaning only for its
      *    (sub-)companion stars.  And, as such, we won't have any
      *    meaningful info here for the first (or only) star of the
      *    star system.
       01  WS-STAR-SEP                 OCCURS 0 TO 200 TIMES
                                       DEPENDING ON STAR-COUNT
                                       INDEXED BY SEP-IDX, PREV-SEP-IDX.
           05  SEP-INFO-USEABLE        PIC X VALUE '-'.
               88  SEP-IS-USEABLE      VALUE 'Y'
                                       WHEN SET TO FALSE IS '-'.
           05  SEP-BETWEEN.
      *            SEP-FROM generally refers to the system's primary,
      *            but can also be referring to e.g. a very distant
      *            companion that has its own orbiting star(s).
               10  SEP-FROM            INDEX.                           STAR-IDX
               10  SEP-TO              INDEX.                           STAR-IDX
           05  SEP-CATEGORY            PIC S9 VALUE -1.
               COPY SEPCATEG.
      *        Distance is the average distance to the primary(* star.
      *        Minimum/maximum distance are derived from this with help
      *        of ORBIT-ECCENTRICITY of their INFO.
           05  SEP-AVG-DISTANCE        USAGE COMP-2.                    AU
           05  ORBIT-ECCENTRICITY      USAGE COMP-2.
       01  WS-TMP-ORBIT-COUNT          PIC 999 USAGE COMP-5 VALUE 0.
       01  WS-TMP-ORBIT                OCCURS 0 TO 200 TIMES
                                       DEPENDING ON WS-TMP-ORBIT-COUNT
                                       INDEXED BY WS-TMP-ORB-IDX.
           05  DISTANCE                USAGE COMP-2.
      *********************************
      * Asteroid belt data.
      *
       01  WS-BELT-COUNT               PIC 999 USAGE COMP-5 VALUE 0.
       01  WS-OBJ-ASTEROID-BELT        OCCURS 0 TO 200 TIMES
                                       DEPENDING ON WS-BELT-COUNT
                                       INDEXED BY BELT-IDX.
           COPY ABELTINF.
      *********************************
      * Terrestrial planet data.
      *
       01  WS-TERRA-COUNT              PIC 999 USAGE COMP-5 VALUE 0.
       01  WS-OBJ-TERRESTRIAL          OCCURS 0 TO 200 TIMES
                                       DEPENDING ON WS-TERRA-COUNT
                                       INDEXED BY TERRA-IDX.
           COPY TERRAINF.
      *********************************
      * Gas giant data.
      *
       01  WS-GG-COUNT                 PIC 999 USAGE COMP-5 VALUE 0.
       01  WS-OBJ-GAS-GIANT            OCCURS 0 TO 200 TIMES
                                       DEPENDING ON WS-GG-COUNT
                                       INDEXED BY GG-IDX.
           COPY GGINF.

       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-PARM-LEN             PIC ZZ9.
           05  LK-PARM-DATA            PIC X(100).

      ******************************************************************
      *    /`--------´\
      *   [    MAIN    ]
      *    ^~~~~~~~~~~^
       PROCEDURE DIVISION USING LK-PARM.
      *    Parse "command line":
           COMPUTE PARM-LEN =
                   FUNCTION LENGTH(FUNCTION TRIM(LK-PARM-DATA)).
           PERFORM UNTIL PARM-INDEX > PARM-LEN
               INITIALIZE PARSED-FIELD
               UNSTRING LK-PARM-DATA   DELIMITED BY ','
                       INTO PARSED-FIELD
                       WITH POINTER PARM-INDEX
               END-UNSTRING
               STRING FUNCTION TRIM(PARSED-FIELD)
                       DELIMITED BY SIZE
                       INTO PARSED-FIELD
               MOVE FUNCTION TRIM(PARSED-FIELD) TO PARSED-FIELD

               EVALUATE TRUE
                   WHEN PARSED-FIELD(1:1) = 'C'
                     OR PARSED-FIELD(1:1) = 'c'
                       SET IN-CLUSTER-OR-CORE TO TRUE
                   WHEN PARSED-FIELD(1:1) = 'V'
                     OR PARSED-FIELD(1:1) = 'v'
                       SET VERBOSE-OUTPUT TO TRUE
               END-EVALUATE
           END-PERFORM.
           
      *    Parse our stellar CSV …
           IF VERBOSE-OUTPUT THEN
               DISPLAY 'Processing CSV: ' NO ADVANCING
           END-IF.
           OPEN INPUT CSV-FILE.
           SET EVO-IDX TO 0.
           PERFORM UNTIL EXIT
               READ CSV-FILE INTO CSV-LINE
                   AT END              EXIT PERFORM
                   NOT AT END          PERFORM PARSE-CSV-LINE
               END-READ

               IF WAS-CSV-ERROR THEN
                   DISPLAY X"0a"'           CSV: ERROR'
                           X"0a"'           with 'CSV-LINE
                   CLOSE CSV-FILE
                   STOP RUN                                              ABORT!
               END-IF
           END-PERFORM.
           CLOSE CSV-FILE.
           IF VERBOSE-OUTPUT THEN
               DISPLAY X"0a"'           CSV: OK'
           END-IF.
       
      *    First things first, the star system's age:
           CALL 'GEN-SYSTEM-AGE' USING SYSTEM-AGE.
           IF VERBOSE-OUTPUT THEN
               DISPLAY 'System age 'BYR' BYr.'
           END-IF.

      *    Second, determine the (initial) number of stars in the
      *    system.  This may change later, depending on if e.g. very
      *    distant companions have their own "local companions".
           COPY 3D6.
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 10 MOVE 1 TO STAR-COUNT
               WHEN D6 IS LESS OR EQUAL TO 15 MOVE 2 TO STAR-COUNT
               WHEN OTHER MOVE 3 TO STAR-COUNT
           END-EVALUATE.
           IF VERBOSE-OUTPUT THEN
               DISPLAY 'Generating a star system with '
                       STAR-COUNT' star(s).'
           END-IF.

      *    Some index priming:
           SET BELT-IDX TO 1.
           SET TERRA-IDX TO 1.
           SET GG-IDX TO 1.
      *    Third, generate the star(s).
           SET SEP-IDX TO 1.
           SET PREV-SEP-IDX TO SEP-IDX.
           SET PREV-SEP-IDX DOWN BY 1.
           SET STAR-IDX TO 1.
           SET PARENT-STAR-IDX TO STAR-IDX.
           PERFORM VARYING STAR-IDX FROM 1 BY 1
                   UNTIL STAR-IDX > STAR-COUNT
               IF STAR-IDX > 1 THEN DISPLAY '-~→ 'STAR-IDX END-IF
               IF SEP-IDX = 1 THEN
                    SET SEP-IS-USEABLE(SEP-IDX) TO FALSE
               ELSE SET SEP-IS-USEABLE(SEP-IDX) TO TRUE
               END-IF

               PERFORM GENERATE-STAR
               
               PERFORM DETERMINE-ORBITAL-INFO
               IF SEP-IS-USEABLE(SEP-IDX) THEN
                   PERFORM DISPLAY-ORBITAL-INFO
               END-IF
               
               CALL 'GEN-GAS-GIANT-ARRANGEMENT' USING
                                       ORBIT-LIMITS(STAR-IDX),
                                       GAS-GIANT-ARRANGEMENT(STAR-IDX)
               DISPLAY 'Gas giant arrangement: 'ARRANGEMENT(STAR-IDX)
               
               PERFORM DETERMINE-ORBITS
               PERFORM PLACE-PLANETS-AND-BELTS

               SET SEP-IDX UP BY 1
               SET PREV-SEP-IDX UP BY 1
           END-PERFORM.

      *-----------------------------------------------------------------,
      *****                                                             D------,
      *****    /`-----------´\                                          | GAME |
      *****   [ 0_0 END MAIN  ] -~=>       {:THE END:}                  | OVER |
      **/      )  ( `¨¨¨¨¨¨¨¨´                                          | MAN! |
           GOBACK.                                                      D------´
      ******************************************************************´
      *
      *********************************
      * Parse a line of CSV.                                            p.103
      *                                                                 p.126
       PARSE-CSV-LINE.
           SET WAS-CSV-COMMENT TO FALSE
      *    Lines beginning with '#' are treated as comments and thus    NOTE
      *    they are entirely skipped, except for X number of potential
      *    "control comments":
           EVALUATE TRUE
               WHEN CSV-LINE(1:1) = '#'
                   IF VERBOSE-OUTPUT THEN
                       DISPLAY '#' NO ADVANCING
                   END-IF
                   SET WAS-CSV-COMMENT TO TRUE
                   EXIT PARAGRAPH
               WHEN CSV-LINE(1:1) = 'M'
                   IF VERBOSE-OUTPUT THEN
                       DISPLAY 'm' NO ADVANCING
                   END-IF
                   PERFORM PARSE-CSV-LINE-M
               WHEN OTHER
                   IF VERBOSE-OUTPUT THEN
                       DISPLAY '.' NO ADVANCING
                   END-IF
                   PERFORM PARSE-CSV-LINE-N
           END-EVALUATE
           EXIT PARAGRAPH.

       PARSE-CSV-LINE-M.                                                p.126
      *    Parse massive star stuff.
           UNSTRING CSV-LINE DELIMITED BY ',' INTO
                   WS-TMP-STR
                   WS-CSV-MASS
                   WS-CSV-L-MIN
                   WS-CSV-AVG-TEMP
                   WS-CSV-S-SPAN
                   ON OVERFLOW
                       SET WAS-CSV-ERROR TO TRUE
                       MOVE 112 TO RETURN-CODE
                       EXIT PARAGRAPH.
           SET EVO-IDX UP BY 1
           ADD 1 TO EVO-COUNT

           COMPUTE MASS OF STELLAR-EVO(EVO-IDX) =
               FUNCTION NUMVAL( FUNCTION TRIM( WS-CSV-MASS ))
           COMPUTE LUMINOSITY-MIN(EVO-IDX) =                            Flat lum
               FUNCTION NUMVAL( FUNCTION TRIM( WS-CSV-L-MIN ))          actually
           COMPUTE AVG-TEMP(EVO-IDX) =
               FUNCTION NUMVAL( FUNCTION TRIM( WS-CSV-AVG-TEMP ))
           COMPUTE SPAN-S(EVO-IDX) =                                    Stable
               FUNCTION NUMVAL( FUNCTION TRIM( WS-CSV-S-SPAN ))         lifespan
           EXIT PARAGRAPH.

       PARSE-CSV-LINE-N.                                                p.103
      *    Parse normie star stuff.
           UNSTRING CSV-LINE DELIMITED BY ',' INTO
                   WS-CSV-MASS OF WS-EVO-CSV
                   WS-CSV-APPROX-TYPE
                   WS-CSV-AVG-TEMP OF WS-EVO-CSV
                   WS-CSV-L-MIN
                   WS-CSV-L-MAX
                   WS-CSV-M-SPAN
                   WS-CSV-S-SPAN OF WS-EVO-CSV
                   WS-CSV-G-SPAN
                   ON OVERFLOW
                       SET WAS-CSV-ERROR TO TRUE
                       MOVE 112 TO RETURN-CODE
                       EXIT PARAGRAPH.
           
           SET EVO-IDX UP BY 1
           ADD 1 TO EVO-COUNT OF WS-EVO-REC

           COMPUTE MASS OF WS-EVO-REC(EVO-IDX) =                        Mass
               FUNCTION NUMVAL(
                FUNCTION TRIM(
                   WS-CSV-MASS OF WS-EVO-CSV ))
           
           MOVE WS-CSV-APPROX-TYPE TO APPROX-TYPE(EVO-IDX)              Approx.T
           
           COMPUTE AVG-TEMP OF WS-EVO-REC(EVO-IDX) =
               FUNCTION NUMVAL(
                FUNCTION TRIM(
                   WS-CSV-AVG-TEMP OF WS-EVO-CSV ))
           
           COMPUTE LUMINOSITY-MIN OF WS-EVO-REC(EVO-IDX) =              L-Min
               FUNCTION NUMVAL( FUNCTION TRIM(WS-CSV-L-MIN) )
           
           IF  FUNCTION TRIM(WS-CSV-L-MAX) = '-' THEN                   L-Max
               MOVE NOT-APPLICABLE TO LUMINOSITY-MAX(EVO-IDX)
           ELSE COMPUTE LUMINOSITY-MAX(EVO-IDX) =
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-L-MAX)).
           
           IF  FUNCTION TRIM(WS-CSV-M-SPAN) = '-' THEN                  Main-
               MOVE NOT-APPLICABLE TO SPAN-M(EVO-IDX)                   sequence
           ELSE COMPUTE SPAN-M(EVO-IDX) =                               lifespan
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-M-SPAN)).
           
           IF  FUNCTION TRIM(WS-CSV-S-SPAN OF WS-EVO-CSV) = '-' THEN    Subgiant
               MOVE NOT-APPLICABLE                                      stage
                 TO SPAN-S OF WS-EVO-REC(EVO-IDX)                       lifespan
           ELSE COMPUTE SPAN-S OF WS-EVO-REC(EVO-IDX) =                 
                   FUNCTION NUMVAL(
                    FUNCTION TRIM(
                        WS-CSV-S-SPAN OF WS-EVO-CSV )).
           
           IF  FUNCTION TRIM(WS-CSV-G-SPAN) = '-' THEN                  Giant
               MOVE NOT-APPLICABLE TO SPAN-G(EVO-IDX)                   stage
           ELSE COMPUTE SPAN-G(EVO-IDX) =                               lifespan
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-G-SPAN)).
           
           EXIT PARAGRAPH.

      *********************************
      * We'll generate a star here, obviously.
      *
       GENERATE-STAR.
           CALL 'GEN-SRCH-MASS' USING  STAR(STAR-IDX).                  Mass
           DISPLAY 'Star mass 'MASS OF STAR(STAR-IDX).
           
           CALL 'GET-MASS-INDEX' USING MASS OF STAR(STAR-IDX)           massidx*
                                       WS-EVO-REC
                                       STAR(STAR-IDX).
      D    DISPLAY ' ⇢ index 'MASS-INDEX(STAR-IDX).
      D    DISPLAY ' ⇢ massive 'MASS-STAGE(STAR-IDX).
           
           CALL 'DETERMINE-LIFE-STAGE' USING SYSTEM-AGE                 stage
                       STELLAR-EVO OF WS-EVO-REC(MASS-INDEX(STAR-IDX))
                                       STAR(STAR-IDX).
           DISPLAY 'Stage 'STAGE(STAR-IDX).

           CALL 'DETERMINE-LUMINOSITY' USING SYSTEM-AGE                 lum
                       STELLAR-EVO OF WS-EVO-REC(MASS-INDEX(STAR-IDX))
                                       STAR(STAR-IDX).
           MOVE 5 TO WS-FMT-DIGITS.
           CALL 'FMT-NUM' USING        LUMINOSITY(STAR-IDX),
                                       WS-FMT-DIGITS, WS-TMP-STR.
           DISPLAY 'Luminosity 'FUNCTION TRIM(WS-TMP-STR)' × Sol'.

           CALL 'DETERMINE-STAR-K'     USING SYSTEM-AGE                 surf. K
                       STELLAR-EVO OF WS-EVO-REC(MASS-INDEX(STAR-IDX))
                                       STAR(STAR-IDX).
           DISPLAY 'Surface temperature 'TEMPERATURE(STAR-IDX)'K'.
           
           CALL 'DETERMINE-RADIUS' USING STAR(STAR-IDX).                 rad AU
           MOVE 5 TO WS-FMT-DIGITS.
           CALL 'FMT-NUM' USING        RADIUS OF STAR(STAR-IDX),
                                       WS-FMT-DIGITS, WS-TMP-STR.
           DISPLAY 'Radius 'FUNCTION TRIM(WS-TMP-STR)' AU'.

           PERFORM DETERMINE-ORBIT-LIMITS.
           EXIT PARAGRAPH.

       DETERMINE-ORBITAL-INFO.
      *    3rd in a trinary is, of course, further away than the other
      *    companion.
           IF SEP-IDX = 1 THEN
               SET SEP-IS-USEABLE(SEP-IDX) TO FALSE
               EXIT PARAGRAPH
           ELSE SET SEP-IS-USEABLE(SEP-IDX) TO TRUE.

      *    SEP-IDX 3+ are treated as third-or-beyond in a trinary (or
      *    larger) star system.
           IF SEP-IDX > 2 THEN SET IS-THIRD-IN-TRINARY TO TRUE
           ELSE SET IS-THIRD-IN-TRINARY TO FALSE.

      *    STAR-IDX and PARENT-STAR-IDX are relevant only for companion
      *    star(s) — for the primary star of the system they're utterly
      *    irrelevant/useless.
           IF SEP-IDX > 1 THEN
               SET SEP-TO(SEP-IDX) TO STAR-IDX
               SET SEP-FROM(SEP-IDX) TO PARENT-STAR-IDX
           END-IF.

           CALL 'GENERATE-ORBITAL-SEP-CATEGORY' USING
                                       WS-THIRD-IN-TRINARY,
                                       SEP-CATEGORY(SEP-IDX).
           CALL 'GENERATE-ORBIT-DISTANCE' USING
                                       SEP-CATEGORY(SEP-IDX),
                                       SEP-AVG-DISTANCE(SEP-IDX)
           CALL 'DETERMINE-ORBITAL-ECCENTRICITY' USING
                                       ORBIT-ECCENTRICITY(SEP-IDX)
           EXIT PARAGRAPH.
           
       DETERMINE-ORBIT-LIMITS.
      *    First, inner limit:
           COMPUTE WS-TMP-N0 = 0.1 * MASS OF STAR(STAR-IDX).
           COMPUTE WS-TMP-N1 =
                   0.01 * FUNCTION SQRT(CURRENT-LUM(STAR-IDX)).
           IF WS-TMP-N0 > WS-TMP-N1 THEN
                MOVE WS-TMP-N0
                  TO INNER-LIMIT OF ORBIT-LIMITS(STAR-IDX)
           ELSE MOVE WS-TMP-N1
                  TO INNER-LIMIT OF ORBIT-LIMITS(STAR-IDX).
      D    DISPLAY 'Inner-limit at '
      D            INNER-LIMIT OF ORBIT-LIMITS(STAR-IDX)' AU'
      *    Second, outer limit.  This is based on current mass.
           COMPUTE OUTER-LIMIT OF ORBIT-LIMITS(STAR-IDX) =
                   40 * MASS OF STAR(STAR-IDX).
      D    DISPLAY 'Outer-limit at '
      D            OUTER-LIMIT OF ORBIT-LIMITS(STAR-IDX)' AU'
      *    Third, snow line.  This is based on initial mass while the
      *    star was in main sequence — the distance from the star at
      *    which water ice could exist ~during~ planetary formation.
           COMPUTE SNOW-LINE(STAR-IDX) =
                   4.85 * FUNCTION SQRT(INITIAL-LUM(STAR-IDX)).
      D    DISPLAY 'Snow-line at 'SNOW-LINE(STAR-IDX)' AU'
           EXIT PARAGRAPH.

       DISPLAY-ORBITAL-INFO.
           MOVE 2 TO WS-FMT-DIGITS.
           CALL 'FMT-NUM' USING        SEP-AVG-DISTANCE(SEP-IDX),
                                       WS-FMT-DIGITS, WS-TMP-STR.
           DISPLAY 'Avg. distance to parent '
                   FUNCTION TRIM(WS-TMP-STR)' AU with '
                   NO ADVANCING.
           CALL 'FMT-NUM' USING        ORBIT-ECCENTRICITY(SEP-IDX),
                                       WS-FMT-DIGITS, WS-TMP-STR.
           DISPLAY FUNCTION TRIM(WS-TMP-STR)
                   ' eccentricity (' NO ADVANCING
           COMPUTE WS-TMP-N0 =
                   (1 - ORBIT-ECCENTRICITY(SEP-IDX)) *
                   SEP-AVG-DISTANCE(SEP-IDX).
           CALL 'FMT-NUM' USING        WS-TMP-N0, WS-FMT-DIGITS,
                                       WS-TMP-STR.
           DISPLAY FUNCTION TRIM(WS-TMP-STR)' → ' NO ADVANCING.
           COMPUTE WS-TMP-N0 =
                   (1 + ORBIT-ECCENTRICITY(SEP-IDX)) *
                   SEP-AVG-DISTANCE(SEP-IDX).
           CALL 'FMT-NUM' USING        WS-TMP-N0, WS-FMT-DIGITS,
                                       WS-TMP-STR.
           DISPLAY FUNCTION TRIM(WS-TMP-STR)').'.
           EXIT PARAGRAPH.

       DETERMINE-ORBITS.                                                pp.108-
      *    First we count inward from GGA (or outer-limit if no            109
      *    central GG is present).
      D    DISPLAY '...calculating orbit distances from ' NO ADVANCING.
      D    MOVE 2 TO WS-FMT-DIGITS.
           IF NO-GAS-GIANT(STAR-IDX) THEN
               COPY 1D6.
               COMPUTE WS-TMP-N0 = ((D6 * 0.05) + 1)
                     / OUTER-LIMIT OF ORBIT-LIMITS(STAR-IDX)
           ELSE
               MOVE DISTANCE OF GAS-GIANT-ARRANGEMENT(STAR-IDX)
                 TO WS-TMP-N0
           END-IF.
      D    CALL 'FMT-NUM' USING WS-TMP-N0, WS-FMT-DIGITS, WS-TMP-STR.
      D    DISPLAY FUNCTION TRIM(WS-TMP-STR)' AU inwards:'.
           
           SET WS-TMP-ORB-IDX TO 1
           MOVE 1 TO WS-TMP-ORBIT-COUNT
           MOVE 1 TO NUM-ORBITS(STAR-IDX)
      *    WS-TMP-N0 will hold the "current distance".
           MOVE WS-TMP-N0 TO DISTANCE OF WS-TMP-ORBIT(WS-TMP-ORB-IDX)
           MOVE WS-TMP-N0 TO WS-TMP-N1
           PERFORM VARYING WS-TMP-ORB-IDX FROM 2 BY 1 UNTIL 1 = 2       !forever
      D        CALL 'FMT-NUM' USING    WS-TMP-N0, WS-FMT-DIGITS,
      D                                WS-TMP-STR
      D        DISPLAY '     F ⇢ ~'FUNCTION TRIM(WS-TMP-STR)
      D                NO ADVANCING
               COMPUTE WS-RATIO = COPY ORBSPCFN.
      D        CALL 'FMT-NUM' USING    WS-RATIO, WS-FMT-DIGITS,
      D                                WS-TMP-STR
      D        DISPLAY ' r/~'FUNCTION TRIM(WS-TMP-STR) NO ADVANCING
      *        WS-TMP-N1 will hold the final distance (in AU).
               COMPUTE WS-TMP-N1 = WS-TMP-N1 / WS-RATIO
      *        Calculate the delta and see if it's too small…
               COMPUTE WS-DELTA = WS-TMP-N0 - WS-TMP-N1
               IF WS-DELTA < MIN-ORBIT-GAP THEN
      D            DISPLAY ' d/(using 'MIN-ORBIT-GAP')' NO ADVANCING
                   COMPUTE WS-TMP-N1 = WS-TMP-N0 - MIN-ORBIT-GAP
               ELSE
      D            CALL 'FMT-NUM' USING WS-DELTA, WS-FMT-DIGITS,
      D                                WS-TMP-STR
      D            DISPLAY ' d/~'FUNCTION TRIM(WS-TMP-STR) NO ADVANCING
               END-IF
               IF WS-TMP-N1 < INNER-LIMIT OF ORBIT-LIMITS(STAR-IDX)
                   THEN
      D            DISPLAY '!  would be inside inner-limit …'
                   EXIT PERFORM
               END-IF

               ADD 1 TO WS-TMP-ORBIT-COUNT
               ADD 1 TO NUM-ORBITS(STAR-IDX)
               MOVE WS-TMP-N1
                 TO DISTANCE OF WS-TMP-ORBIT(WS-TMP-ORB-IDX)
      D        DISPLAY ' = 'DISTANCE OF WS-TMP-ORBIT(WS-TMP-ORB-IDX)
      *        New 'current distance' for closing up toward the star.
               MOVE WS-TMP-N1 TO WS-TMP-N0
           END-PERFORM.

           SET WS-TMP-ORB-IDX DOWN BY 1                                 - offset
      *    Start filling the orbit distances to actual storage in
      *    reverse order of the above generation phase.
           PERFORM VARYING ORB-IDX
                           FROM 1 BY 1
                           UNTIL WS-TMP-ORBIT-COUNT = 0
               MOVE CORR WS-TMP-ORBIT(WS-TMP-ORB-IDX)
                      TO ORBIT(STAR-IDX, ORB-IDX)
               SET WS-TMP-ORB-IDX DOWN BY 1
               SUBTRACT 1 FROM WS-TMP-ORBIT-COUNT
           END-PERFORM.

      *    Now the ORBIT outward from the "central orbit".  Naturally,
      *    we abort the building phase when outer limit is reached —
      *    anything beyond that would either not form at all or would be
      *    flung out from the system altogether.
           SET DST-ORB-IDX TO ORB-IDX.
           SET ORB-IDX DOWN BY 1.
      *    Mark the 1st GG, if present.
           IF NOT NO-GAS-GIANT(STAR-IDX) THEN
      D        DISPLAY 'Plugging in a GG at ('STAR-IDX','ORB-IDX').'
               PERFORM GEN-GAS-GIANT
           END-IF
      D    DISPLAY '...calculating orbit distances from ' NO ADVANCING.
      D    CALL 'FMT-NUM' USING DISTANCE OF ORBIT(STAR-IDX, ORB-IDX),
      D                         WS-FMT-DIGITS, WS-TMP-STR.
      D    DISPLAY FUNCTION TRIM(WS-TMP-STR)' AU outwards:'.
           MOVE 0.0 TO WS-TMP-N0
           PERFORM VARYING DST-ORB-IDX
                           FROM DST-ORB-IDX BY 1
                           UNTIL WS-TMP-N0 > OUTER-LIMIT
                                             OF ORBIT-LIMITS(STAR-IDX)
      *                          OR EXIT
               COMPUTE WS-DELTA = COPY ORBSPCFN.
               COMPUTE WS-TMP-N0
                     = DISTANCE OF ORBIT(STAR-IDX, ORB-IDX)
                     * WS-DELTA
      D        CALL 'FMT-NUM' USING
      D                    DISTANCE OF ORBIT(STAR-IDX, ORB-IDX),
      D                    WS-FMT-DIGITS, WS-TMP-STR
      D        DISPLAY '     F ⇢ ~'FUNCTION TRIM(WS-TMP-STR)
      D                NO ADVANCING
      D        CALL 'FMT-NUM' USING WS-DELTA, WS-FMT-DIGITS, WS-TMP-STR
      D        DISPLAY ' r/~'FUNCTION TRIM(WS-TMP-STR) NO ADVANCING
               IF WS-TMP-N0 > OUTER-LIMIT OF ORBIT-LIMITS(STAR-IDX)
                   THEN
      D            DISPLAY '!  would exceed outer-limit …'
                   EXIT PERFORM
               END-IF

      D        DISPLAY ' = 'WS-TMP-N0
               MOVE WS-TMP-N0
                 TO DISTANCE OF ORBIT(STAR-IDX, DST-ORB-IDX)
               SET ORB-IDX UP BY 1
               ADD 1 TO NUM-ORBITS(STAR-IDX)
           END-PERFORM.
           EXIT PARAGRAPH.

       PLACE-PLANETS-AND-BELTS.                                         p.110
      *    Placing planets begins with gas giants (that is, if there is
      *    any to begin with).
           IF NOT NO-GAS-GIANT(STAR-IDX) THEN
               PERFORM PLACE-GAS-GIANTS
           END-IF.
      *    And now we place the other thingies into orbit(s), if any
      *    present.  Some of the orbits may well be empty.
           PERFORM INIT-ALL-ORB-IDX.
           PERFORM VARYING ORB-IDX FROM 1 BY 1
                   UNTIL ORB-IDX > NUM-ORBITS(STAR-IDX)
               IF NOT OBJ-NOTHING(STAR-IDX, ORB-IDX)                    Skip the
                   CONTINUE                                             occupied
               END-IF                                                   orbits.
               COPY 3D6.
               IF OBJ-GAS-GIANT(STAR-IDX, NEXT-ORB-IDX) THEN
                   COMPUTE D6 = D6 - 6
               END-IF
               IF OBJ-GAS-GIANT(STAR-IDX, PREV-ORB-IDX) THEN
                   COMPUTE D6 = D6 - 3
               END-IF
               
           END-PERFORM.
           EXIT PARAGRAPH.

       PLACE-GAS-GIANTS.
           COMPUTE WS-COUNT = NUM-ORBITS(STAR-IDX).
           SET ORB-IDX TO 1.
           PERFORM VARYING WS-COUNT
                   FROM WS-COUNT BY -1
                   UNTIL WS-COUNT = 0
      *        Skip any already defined celestial object(s).
               IF NOT OBJ-NOTHING(STAR-IDX, ORB-IDX) THEN
                   PERFORM SYNC-ADD-1-TO-ORB-IDX
                   CONTINUE
               END-IF

               MOVE DISTANCE OF ORBIT(STAR-IDX, ORB-IDX) TO WS-DISTANCE
               MOVE SNOW-LINE OF STAR(STAR-IDX) TO WS-SNOW-LINE
               
               COPY 3D6.
               SET IS-GG TO FALSE
               EVALUATE TRUE
                   WHEN CONVENTIONAL-GG(STAR-IDX)
                       IF (WS-DISTANCE >= WS-SNOW-LINE) AND (D6 <= 15)
                           SET OBJ-GAS-GIANT(STAR-IDX, ORB-IDX) TO TRUE
                       END-IF
                   WHEN ECCENTRIC-GG(STAR-IDX)
                       IF ((WS-DISTANCE < WS-SNOW-LINE) AND (D6 <= 8))
                       OR ((WS-DISTANCE >= WS-SNOW-LINE) AND (D6 <= 14))
                           SET OBJ-GAS-GIANT(STAR-IDX, ORB-IDX) TO TRUE
                       END-IF
                   WHEN EPISTELLAR-GG(STAR-IDX)
                       IF ((WS-DISTANCE < WS-SNOW-LINE) AND (D6 <= 6))
                       OR ((WS-DISTANCE >= WS-SNOW-LINE) AND (D6 <= 14))
                           SET OBJ-GAS-GIANT(STAR-IDX, ORB-IDX) TO TRUE
                       END-IF
               END-EVALUATE
               PERFORM SYNC-ADD-1-TO-ORB-IDX
           END-PERFORM.
           EXIT PARAGRAPH.

       GEN-GAS-GIANT.
           SET OBJ-GAS-GIANT(STAR-IDX, ORB-IDX) TO TRUE.
           SET OBJ-REF(STAR-IDX, ORB-IDX) TO GG-IDX.
           
           COPY 3D6.
           IF DISTANCE OF ORBIT(STAR-IDX, ORB-IDX)
              < SNOW-LINE(STAR-IDX) THEN                                TODO
      *        TODO: check whether 1st orbit beyond snow line.
               COMPUTE D6 = D6 + 4
           END-IF
           EVALUATE TRUE
               WHEN D6 <= 10
                   SET GG-SMALL(GG-IDX) TO TRUE
               WHEN D6 <= 16
                   SET GG-MEDIUM(GG-IDX) TO TRUE
               WHEN OTHER
                   SET GG-LARGE(GG-IDX) TO TRUE
           END-EVALUATE.

           SET GG-IDX UP BY 1.                                          4-future
           EXIT PARAGRAPH.

       SYNC-ALL-ORB-IDX.
           SET PREV-ORB-IDX TO ORB-IDX.
           SET NEXT-ORB-IDX TO ORB-IDX.
           SET PREV-ORB-IDX DOWN BY 1.
           SET NEXT-ORB-IDX UP BY 1.
           EXIT PARAGRAPH.

       SYNC-ADD-1-TO-ORB-IDX.
      *    For sync'ing to function properly, PREV-/NEXT-ORB-IDX have to
      *    be pre-synced with e.g. SYNC-ALL-ORB-IDX or INIT-ALL-ORB-IDX.
           SET ORB-IDX UP BY 1.
           SET PREV-ORB-IDX UP BY 1.
           SET NEXT-ORB-IDX UP BY 1.
           EXIT PARAGRAPH.

       SYNC-SUBTRACT-1-FROM-ORB-IDX.
      *    For sync'ing to function properly, PREV-/NEXT-ORB-IDX have to
      *    be pre-synced with e.g. SYNC-ALL-ORB-IDX or INIT-ALL-ORB-IDX.
           SET ORB-IDX DOWN BY 1.
           SET PREV-ORB-IDX DOWN BY 1.
           SET NEXT-ORB-IDX DOWN BY 1.
           EXIT PARAGRAPH.

       INIT-ALL-ORB-IDX.
           SET ORB-IDX TO 1.
           PERFORM SYNC-ALL-ORB-IDX.
           EXIT PARAGRAPH.

       CHECK-LIMIT-ADJACENCY.
       