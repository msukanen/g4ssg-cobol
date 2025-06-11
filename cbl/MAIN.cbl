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
      *********************************
      * Misc. variables/constants.
       01  WS-WAS-CSV-COMMENT          PIC X VALUE 'N'.
           88  WAS-CSV-COMMENT         VALUE 'Y'
                                       WHEN SET TO FALSE IS 'N'.
      *    88  WAS-NOT-CSV-COMMENT     VALUE 'N'.
       01  WS-WAS-CSV-ERROR            PIC X VALUE 'N'.
           88  WAS-CSV-ERROR           VALUE 'Y'
                                       WHEN SET TO FALSE IS 'N'.
      *    88  NO-CSV-ERROR            VALUE 'N'.
       COPY CONST.
      * ... 200 as maximum number of stars is probably overkill ...
       77  MAX-STARS                   PIC 999 VALUE 200.               CONSTANT
       77  MAX-ORBITS                  PIC 999 VALUE 200.               CONSTANT
      * System generation basics.
       01  WS-IN-CLUSTER-OR-CORE       PIC X VALUE 'N'.
           88  IN-CLUSTER-OR-CORE      VALUE 'Y'
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
                                       INDEXED BY STAR-IDX.
               COPY STARDATA.
      *    Note that separation data ~should~ be ignored for the first    H0X!
      *    star in the system, entirely — it has meaning only for its
      *    (sub-)companion stars.  And, as such, we won't have any
      *    meaningful info here for the first (or only) star of the
      *    star system.
       01  WS-STAR-SEPARATION.
           COPY STARSEP.

       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-P-LEN                PIC ZZ9.
           05  LK-P-DATA               PIC X(100).

      ******************************************************************
      *    /`--------´\
      *   [    MAIN    ]
      *    ^~~~~~~~~~~^
       PROCEDURE DIVISION USING LK-PARM.
      *    Parse "command line":
           COMPUTE PARM-LEN = FUNCTION LENGTH(FUNCTION TRIM(LK-P-DATA))
           PERFORM UNTIL PARM-INDEX > PARM-LEN
               INITIALIZE PARSED-FIELD
               UNSTRING LK-P-DATA      DELIMITED BY ','
                                       INTO PARSED-FIELD
                                       WITH POINTER PARM-INDEX
               END-UNSTRING
               STRING FUNCTION TRIM(PARSED-FIELD)
                                       DELIMITED BY SIZE
                                       INTO PARSED-FIELD
               MOVE FUNCTION TRIM(PARSED-FIELD) TO PARSED-FIELD

               IF PARSED-FIELD(1:1) = 'C' THEN
                   SET IN-CLUSTER-OR-CORE TO TRUE
               END-IF
           END-PERFORM
           
      *    Parse our stellar CSV...:
           OPEN INPUT CSV-FILE
           SET EVO-IDX TO 0
           DISPLAY 'Processing CSV: ' NO ADVANCING
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
           END-PERFORM
           CLOSE CSV-FILE
           DISPLAY X"0a"'           CSV: OK'.
       
      *    First things first, the star system's age:
           CALL 'GEN-SYSTEM-AGE' USING SYSTEM-AGE
           DISPLAY 'System age 'BYR' BYr.'

      *    Second, determine the (initial) number of stars in the
      *    system.  This may change later, depending on if e.g. very
      *    distant companions have their own "local companions".
           COPY 3D6.
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 10 MOVE 1 TO STAR-COUNT
               WHEN D6 IS LESS OR EQUAL TO 15 MOVE 2 TO STAR-COUNT
               WHEN OTHER MOVE 3 TO STAR-COUNT
           END-EVALUATE
           DISPLAY 'Generating star system with 'STAR-COUNT' star(s).'

      *    Third, generate the star(s).
           SET SEP-IDX TO 1
           SET PREV-SEP-IDX TO SEP-IDX
           SET PREV-SEP-IDX DOWN BY 1
           PERFORM VARYING STAR-IDX FROM 1 BY 1
                   UNTIL STAR-IDX > STAR-COUNT
               IF STAR-IDX > 1 THEN
                   DISPLAY '---'STAR-IDX
               END-IF
               
               PERFORM GENERATE-STAR
               PERFORM DETERMINE-ORBITAL-SEPARATION

               SET SEP-IDX UP BY 1
               SET PREV-SEP-IDX UP BY 1
           END-PERFORM

      *-----------------------------------------------------------------,
      *                                                                 D------,
      *    /`-----------´\                                              | END  |
      *   [ 0_0 END MAIN  ] -~=>       {:THE END:}                      | MAIN |
      *    )  ( `¨¨¨¨¨¨¨¨´                                              | CRAZE|
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
                   DISPLAY '#' NO ADVANCING
                   SET WAS-CSV-COMMENT TO TRUE
                   EXIT PARAGRAPH
               WHEN CSV-LINE(1:1) = 'M'
                   DISPLAY 'm' NO ADVANCING
                   PERFORM PARSE-CSV-LINE-M
               WHEN OTHER
                   DISPLAY '.' NO ADVANCING
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
           COMPUTE LUMINOSITY-MIN(EVO-IDX) =
               FUNCTION NUMVAL( FUNCTION TRIM( WS-CSV-L-MIN ))
           COMPUTE AVG-TEMP(EVO-IDX) =
               FUNCTION NUMVAL( FUNCTION TRIM( WS-CSV-AVG-TEMP ))
           COMPUTE SPAN-S(EVO-IDX) =
               FUNCTION NUMVAL( FUNCTION TRIM( WS-CSV-S-SPAN ))
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

           COMPUTE MASS OF WS-EVO-REC(EVO-IDX) =
               FUNCTION NUMVAL(
                FUNCTION TRIM(
                   WS-CSV-MASS OF WS-EVO-CSV ))
           
           MOVE WS-CSV-APPROX-TYPE TO APPROX-TYPE(EVO-IDX)
           
           COMPUTE AVG-TEMP OF WS-EVO-REC(EVO-IDX) =
               FUNCTION NUMVAL(
                FUNCTION TRIM(
                   WS-CSV-AVG-TEMP OF WS-EVO-CSV ))
           
           COMPUTE LUMINOSITY-MIN OF WS-EVO-REC(EVO-IDX) =
               FUNCTION NUMVAL( FUNCTION TRIM(WS-CSV-L-MIN) )
           
           IF  FUNCTION TRIM(WS-CSV-L-MAX) = '-' THEN
               MOVE NOT-APPLICABLE TO LUMINOSITY-MAX(EVO-IDX)
           ELSE COMPUTE LUMINOSITY-MAX(EVO-IDX) =
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-L-MAX)).
           
           IF  FUNCTION TRIM(WS-CSV-M-SPAN) = '-' THEN
               MOVE NOT-APPLICABLE TO SPAN-M(EVO-IDX)
           ELSE COMPUTE SPAN-M(EVO-IDX) =
                   FUNCTION NUMVAL(FUNCTION TRIM(WS-CSV-M-SPAN)).
           
           IF  FUNCTION TRIM(WS-CSV-S-SPAN OF WS-EVO-CSV) = '-' THEN
               MOVE NOT-APPLICABLE
                    TO SPAN-S OF WS-EVO-REC(EVO-IDX)
           ELSE COMPUTE SPAN-S OF WS-EVO-REC(EVO-IDX) =
                   FUNCTION NUMVAL(
                    FUNCTION TRIM(
                        WS-CSV-S-SPAN OF WS-EVO-CSV )).
           
           IF  FUNCTION TRIM(WS-CSV-G-SPAN) = '-' THEN
               MOVE NOT-APPLICABLE TO SPAN-G(EVO-IDX)
           ELSE COMPUTE SPAN-G(EVO-IDX) =
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
           CALL 'FMT-NUM' USING LUMINOSITY(STAR-IDX), WS-TMP-STR
           DISPLAY 'Luminosity 'FUNCTION TRIM(WS-TMP-STR)' × Sol'

           CALL 'DETERMINE-STAR-K'     USING SYSTEM-AGE                 surf. K
                       STELLAR-EVO OF WS-EVO-REC(MASS-INDEX(STAR-IDX))
                                       STAR(STAR-IDX)
           DISPLAY 'Surface temperature 'TEMPERATURE(STAR-IDX)'K'
           
           CALL 'DETERMINE-RADIUS' USING STAR(STAR-IDX)                 rad AU
           CALL 'FMT-NUM' USING RADIUS(STAR-IDX), WS-TMP-STR
           DISPLAY 'Radius 'FUNCTION TRIM(WS-TMP-STR)' AU'
           
           EXIT PARAGRAPH.

       DETERMINE-ORBITAL-SEPARATION.
      *    3rd in a trinary is, of course, further away than the other
      *    companion.
           IF SEP-IDX > 2 THEN
                MOVE 'Y' TO WS-TMP-STR
           ELSE MOVE '-' TO WS-TMP-STR.
           IF SEP-IDX > 1 THEN
               CALL 'DETERMINE-ORBITAL-SEP' USING WS-TMP-STR
                                       STAR-COUNT, WS-STAR-SEPARATION
                                       PREV-SEP-IDX, SEP-IDX
           ELSE
      *        We omit the non-existent earlier separation value ;-)
               CALL 'DETERMINE-ORBITAL-SEP' USING WS-TMP-STR,
                                       STAR-COUNT, WS-STAR-SEPARATION
                                       OMITTED, SEP-IDX
           END-IF
           EXIT PARAGRAPH.
           