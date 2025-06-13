       IDENTIFICATION DIVISION.
       PROGRAM-ID.   G4SSGCRE.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 3 — June 9, 2025
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
       01  WS-TMP-STR                  PIC X(100).
       01  WS-TMP-NUM0                 USAGE COMP-2.
       01  WS-TMP-NUM1                 USAGE COMP-2.
       01  WS-TMP-NUM2                 USAGE COMP-2.
      *********************************
      * Parsed run params:
       01  PARSED-PARM.
           05  PARM-LEN                PIC 999 USAGE COMP-3.
           05  PARSED-FIELD            PIC X(20).
           05  PARM-INDEX              VALUE 1 INDEX.
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
               COPY STLRAGE.
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
               DISPLAY 'Generating star system with '
                       STAR-COUNT' star(s).'
           END-IF.

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
                   MOVE 2 TO WS-FMT-DIGITS

                   CALL 'FMT-NUM' USING SEP-AVG-DISTANCE(SEP-IDX),
                                       WS-FMT-DIGITS,
                                       WS-TMP-STR
                   DISPLAY 'Avg. distance to parent '
                           FUNCTION TRIM(WS-TMP-STR)' AU with '
                           NO ADVANCING
                   CALL 'FMT-NUM' USING ORBIT-ECCENTRICITY(SEP-IDX),
                                       WS-FMT-DIGITS,
                                       WS-TMP-STR
                   DISPLAY FUNCTION TRIM(WS-TMP-STR)
                           ' eccentricity (' NO ADVANCING
                   COMPUTE WS-TMP-NUM0 =
                           (1 - ORBIT-ECCENTRICITY(SEP-IDX)) *
                           SEP-AVG-DISTANCE(SEP-IDX)
                   CALL 'FMT-NUM' USING WS-TMP-NUM0, WS-FMT-DIGITS,
                                       WS-TMP-STR
                   DISPLAY FUNCTION TRIM(WS-TMP-STR)' → ' NO ADVANCING
                   COMPUTE WS-TMP-NUM0 =
                           (1 + ORBIT-ECCENTRICITY(SEP-IDX)) *
                           SEP-AVG-DISTANCE(SEP-IDX)
                   CALL 'FMT-NUM' USING WS-TMP-NUM0, WS-FMT-DIGITS,
                                       WS-TMP-STR
                   DISPLAY FUNCTION TRIM(WS-TMP-STR)').'
               END-IF

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
                    TO SPAN-S OF WS-EVO-REC(EVO-IDX)                    lifespan
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
           CALL 'GEN-SRCH-MASS' USING  STAR(STAR-IDX)                   Mass
           DISPLAY 'Star mass 'MASS OF STAR(STAR-IDX)
           
           CALL 'GET-MASS-INDEX' USING MASS OF STAR(STAR-IDX)           massidx*
                                       WS-EVO-REC
                                       STAR(STAR-IDX)
      D    DISPLAY ' ⇢ index 'MASS-INDEX(STAR-IDX)
      D    DISPLAY ' ⇢ massive 'MASS-STAGE(STAR-IDX)
           
           CALL 'DETERMINE-LIFE-STAGE' USING SYSTEM-AGE                 stage
                       STELLAR-EVO OF WS-EVO-REC(MASS-INDEX(STAR-IDX))
                                       STAR(STAR-IDX)
           DISPLAY 'Stage 'STAGE(STAR-IDX)

           CALL 'DETERMINE-LUMINOSITY' USING SYSTEM-AGE                 lum
                       STELLAR-EVO OF WS-EVO-REC(MASS-INDEX(STAR-IDX))
                                       STAR(STAR-IDX)
           MOVE 5 TO WS-FMT-DIGITS
           CALL 'FMT-NUM' USING        LUMINOSITY(STAR-IDX),
                                       WS-FMT-DIGITS, WS-TMP-STR
           DISPLAY 'Luminosity 'FUNCTION TRIM(WS-TMP-STR)' × Sol'

           CALL 'DETERMINE-STAR-K'     USING SYSTEM-AGE                 surf. K
                       STELLAR-EVO OF WS-EVO-REC(MASS-INDEX(STAR-IDX))
                                       STAR(STAR-IDX)
           DISPLAY 'Surface temperature 'TEMPERATURE(STAR-IDX)'K'
           
           CALL 'DETERMINE-RADIUS' USING STAR(STAR-IDX)                 rad AU
           MOVE 5 TO WS-FMT-DIGITS
           CALL 'FMT-NUM' USING        RADIUS(STAR-IDX),
                                       WS-FMT-DIGITS, WS-TMP-STR
           DISPLAY 'Radius 'FUNCTION TRIM(WS-TMP-STR)' AU'
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
               MOVE STAR-IDX TO SEP-TO(SEP-IDX)
               MOVE PARENT-STAR-IDX TO SEP-FROM(SEP-IDX)
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
           COMPUTE WS-TMP-NUM0 = 0.1 * MASS OF STAR(STAR-IDX).
           COMPUTE WS-TMP-NUM1 =
                   0.01 * FUNCTION SQRT(CURRENT-LUM(STAR-IDX)).
           IF WS-TMP-NUM0 > WS-TMP-NUM1 THEN
                MOVE WS-TMP-NUM0
                     TO INNER-LIMIT OF ORBIT-LIMITS(STAR-IDX)
           ELSE MOVE WS-TMP-NUM1
                     TO INNER-LIMIT OF ORBIT-LIMITS(STAR-IDX).
      *    Second, outer limit.  This is based on current mass.
           COMPUTE OUTER-LIMIT OF ORBIT-LIMITS(STAR-IDX) =
                   40 * MASS OF STAR(STAR-IDX).
      *    Third, snow line.  This is based on initial mass while the
      *    star was in main sequence — the distance from the star at
      *    which water ice could exist ~during~ planetary formation.
           COMPUTE SNOW-LINE(STAR-IDX) =
                   4.85 * FUNCTION SQRT(INITIAL-LUM(STAR-IDX)).
           EXIT PARAGRAPH.
