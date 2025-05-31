       IDENTIFICATION DIVISION.
       PROGRAM-ID. G4SSGCRE.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Star system generator, based on "GURPS 4e Space" rules.
      *
      ******************************************************************
       COPY TESTENV.

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
       01  D6                          PIC 999 USAGE COMP-3.            Prim. d6
       01  D62                         PIC 999 USAGE COMP-3.            Scnd. d6
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
           88  WAS-CSV-COMMENT         VALUE 'Y'.
           88  WAS-NOT-CSV-COMMENT     VALUE 'N'.
       COPY CONST.
      * ... 200 as maximum number of stars is probably overkill ...
       77  MAX-STARS                   PIC 999 VALUE 200.               CONSTANT
      * System generation basics.
       01  WS-IN-CLUSTER-OR-CORE       PIC X VALUE 'N'.
           88  IN-CLUSTER-OR-CORE      VALUE 'Y'.
           88  NOT-IN-CLUSTER-OR-CORE  VALUE 'N'.
      *********************************
      * Stellar CSV related:
       77  MAX-EVO                     PIC 99 VALUE 34.                 CONSTANT
       01  WS-EVO-CSV.
           05  WS-CSV-MASS             PIC X(10).
           05  WS-CSV-APPROX-TYPE      PIC X(10).
           05  WS-CSV-AVG-TEMP         PIC X(10).
           05  WS-CSV-L-MIN            PIC X(10).
           05  WS-CSV-L-MAX            PIC X(10).
           05  WS-CSV-M-SPAN           PIC X(10).
           05  WS-CSV-S-SPAN           PIC X(10).
           05  WS-CSV-G-SPAN           PIC X(10).
       01  STELLAR-EVOLUTION
                   OCCURS 34 TIMES                                      ^MAXIMUM
                   INDEXED BY EVO-IDX.
           COPY STLREVO.
       01  WS-STELLAR-POPULATION       PIC XX.
           COPY STLRPOP.
       01  WS-STELLAR-AGE              PIC 99V99.
       01  WS-NUM-STARS                PIC 999 USAGE COMP-3 VALUE 0.
       01  WS-STAR OCCURS 0 TO 200 TIMES                                ^MAXIMUM
                   DEPENDING ON WS-NUM-STARS
                   INDEXED BY STAR-IDX.
           COPY STAR.

       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-P-LEN                PIC ZZ9.
           05  LK-P-DATA               PIC X(100).

       PROCEDURE DIVISION USING LK-PARM.
      *_____________________
      ****[    MAIN    ]****
      *
      * Parse "command line":
           COMPUTE PARM-LEN = FUNCTION LENGTH(FUNCTION TRIM(LK-P-DATA)).
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
           END-PERFORM.
           
      * Parse our stellar CSV...:
           OPEN INPUT CSV-FILE.
           PERFORM VARYING EVO-IDX
                   FROM MAX-EVO BY -1
                   UNTIL EVO-IDX IS EQUAL TO 0
               READ CSV-FILE INTO CSV-LINE
                   NOT AT END
                       PERFORM PARSE-CSV-LINE
               END-READ
               IF WAS-CSV-COMMENT THEN
                   SET EVO-IDX UP BY 1
               END-IF
           END-PERFORM.
           CLOSE CSV-FILE.

      *********************************
      * Generate star system.
      *********************************
      D    DISPLAY '_________________________'
           DISPLAY 'Generating star system...'
           CALL 'DETERMINE-STELLAR-AGE'
               USING WS-STELLAR-POPULATION,
                     WS-STELLAR-AGE.
           PERFORM DETERMINE-NUM-STARS.
           
           SET STAR-IDX TO 1.
           DISPLAY 'INDEX 'STAR-IDX
           PERFORM DETERMINE-STELLAR-MASS.
           CALL 'DETERMINE-SEQUENCE'
               USING STELLAR-EVOLUTION(MASS-INDEX(STAR-IDX)),
                     WS-STELLAR-AGE, STAGE(STAR-IDX).
           PERFORM REFINE-STELLAR-MASS.
           PERFORM DETERMINE-TEMPERATURE.
           CALL 'DETERMINE-LUMINOSITY'
               USING STELLAR-EVOLUTION(MASS-INDEX(STAR-IDX)),
                     WS-STELLAR-AGE, STAGE(STAR-IDX),
                     LUMINOSITY(STAR-IDX).
      *_________________
      ****[END MAIN]****
           GOBACK.

      *********************************
      * Parse a line of CSV.
      *
      * Lines beginning with '#' are treated as comments and thus
      * they are entirely skipped.
      *
       PARSE-CSV-LINE.
           SET WAS-NOT-CSV-COMMENT TO TRUE
      * We just silently skip all comments, ok?
           IF CSV-LINE(1:1) = '#' THEN
               SET WAS-CSV-COMMENT TO TRUE
               EXIT PARAGRAPH
           END-IF
           
      D    DISPLAY 'EVO-IDX 'EVO-IDX

           UNSTRING CSV-LINE
                DELIMITED BY ','
                INTO WS-CSV-MASS
                     WS-CSV-APPROX-TYPE
                     WS-CSV-AVG-TEMP
                     WS-CSV-L-MIN
                     WS-CSV-L-MAX
                     WS-CSV-M-SPAN
                     WS-CSV-S-SPAN
                     WS-CSV-G-SPAN
      *****
      * Validate CSV entries:
      *
      *    Mass first:
           COMPUTE EVO-MASS(EVO-IDX) ROUNDED =                          Mass
                   FUNCTION NUMVAL(
                    FUNCTION TRIM(WS-CSV-MASS)
                   )
      D    DISPLAY '    Mass 'EVO-MASS(EVO-IDX)' × Sol'

      *    Approx. spectral type and brightness.
           MOVE FUNCTION TRIM(WS-CSV-APPROX-TYPE)                       Approx.T
                   TO EVO-APPROX-TYPE(EVO-IDX)
      D    DISPLAY '   A.Typ 'EVO-APPROX-TYPE(EVO-IDX)

      *    Surface temperature, in K.
           MOVE FUNCTION TRIM(WS-CSV-AVG-TEMP)                          Avg-K
                   TO EVO-AVG-TEMP(EVO-IDX)
      D    DISPLAY '       K 'EVO-AVG-TEMP(EVO-IDX)
           
      *    Luminance minimum.
           COMPUTE EVO-L-MIN(EVO-IDX) ROUNDED =                         L-Min
                   FUNCTION NUMVAL(
                    FUNCTION TRIM(WS-CSV-L-MIN)
                   )
      D    DISPLAY '   L-Min 'EVO-L-MIN(EVO-IDX)

      *    Luminance maximum.
      *        A '-' as value means the star is too dim to begin with
      *        for a maximum value to make much sense, and thus it is
      *        marked as NOT-AVAILABLE.
           IF FUNCTION TRIM(WS-CSV-L-MAX) = '-' THEN                    L-Max
               MOVE NOT-AVAILABLE TO EVO-L-MAX(EVO-IDX)
           ELSE COMPUTE EVO-L-MAX(EVO-IDX) ROUNDED =
                   FUNCTION NUMVAL(
                    FUNCTION TRIM(WS-CSV-L-MAX)
                   )
      D        DISPLAY '   L-Max 'EVO-L-MAX(EVO-IDX)
           END-IF
           
      *    Main-sequence life span.
      *        A '-' as value means the star will live on so many
      *        billion (or trillion, or more) years that it might well
      *        be considered virtually "immortal" with an infinite
      *        life span.
           IF FUNCTION TRIM(WS-CSV-M-SPAN) = '-' THEN                   M-Span
               MOVE INF-LIFESPAN TO EVO-M-SPAN(EVO-IDX)
           ELSE COMPUTE EVO-M-SPAN(EVO-IDX) ROUNDED =
                   FUNCTION NUMVAL(
                    FUNCTION TRIM(WS-CSV-M-SPAN)
                   )
      D        DISPLAY '    Span 'EVO-M-SPAN(EVO-IDX) NO ADVANCING
           END-IF

      *    Subgiant phase life span.
      *        A '-' as value means the star will not enter subgiant
      *        phase at all but instead whizzles quietly into a white
      *        dwarf.
           IF FUNCTION TRIM(WS-CSV-S-SPAN) = '-' THEN                   S-Span
               MOVE NOT-AVAILABLE TO EVO-S-SPAN(EVO-IDX)
           ELSE COMPUTE EVO-S-SPAN(EVO-IDX) ROUNDED =
                   FUNCTION NUMVAL(
                    FUNCTION TRIM(WS-CSV-S-SPAN)
                   )
      D        DISPLAY ' → 'EVO-S-SPAN(EVO-IDX) NO ADVANCING
           END-IF

      *    Giant phase life span.
      *        A '-' as value means the star will not enter giant phase
      *        at all but instead evolves directly into either black
      *        hole or white dwarf.
           IF FUNCTION TRIM(WS-CSV-G-SPAN) = '-' THEN                   G-Span
               MOVE NOT-AVAILABLE TO EVO-G-SPAN(EVO-IDX)
           ELSE COMPUTE EVO-G-SPAN(EVO-IDX) ROUNDED =
                   FUNCTION NUMVAL(
                    FUNCTION TRIM(WS-CSV-G-SPAN)
                   )
      D        DISPLAY ' → 'EVO-G-SPAN(EVO-IDX) NO ADVANCING
           END-IF

      D    IF INF-LIFESPAN <> EVO-M-SPAN(EVO-IDX) THEN
      D        DISPLAY SPACE
      D    END-IF
           EXIT PARAGRAPH.

      *********************************
      * Determine stellar mass (randomly).
      *
      * See 'Stellar Mass Table', 4eSpace.p.101.
      *
       DETERMINE-STELLAR-MASS.
           CALL '3D6' USING D6
           CALL '3D6' USING D62
           SET EVO-IDX TO MAX-EVO
           EVALUATE TRUE
               WHEN D6 = 3
                   IF D62 IS GREATER THAN 10 THEN
                       SET EVO-IDX DOWN BY 1
                   END-IF
               WHEN D6 = 4 OR D6 = 9 OR D6 = 10
                   EVALUATE TRUE
                       WHEN D62 IS LESS OR EQUAL TO 8
                           SET EVO-IDX DOWN BY 2
                       WHEN D62 IS LESS OR EQUAL TO 11
                           SET EVO-IDX DOWN BY 3
                       WHEN OTHER
                           SET EVO-IDX DOWN BY 4
                   END-EVALUATE
                   EVALUATE TRUE
                       WHEN D6 = 9 SET EVO-IDX DOWN BY 23
                       WHEN D6 = 10 SET EVO-IDX DOWN BY 26
                   END-EVALUATE
               WHEN D6 = 5
                   EVALUATE TRUE
                       WHEN D62 IS LESS OR EQUAL TO 7
                           SET EVO-IDX DOWN BY 5
                       WHEN D62 IS LESS OR EQUAL TO 10
                           SET EVO-IDX DOWN BY 6
                       WHEN D62 IS LESS OR EQUAL TO 12
                           SET EVO-IDX DOWN BY 7
                       WHEN OTHER
                           SET EVO-IDX DOWN BY 8
                   END-EVALUATE
               WHEN D6 = 6 OR D6 = 7 OR D6 = 8
                   EVALUATE TRUE
                       WHEN D62 IS LESS OR EQUAL TO 7
                           SET EVO-IDX DOWN BY 9
                       WHEN D62 IS LESS OR EQUAL TO 9
                           SET EVO-IDX DOWN BY 10
                       WHEN D62 = 10
                           SET EVO-IDX DOWN BY 11
                       WHEN D62 IS LESS OR EQUAL TO 12
                           SET EVO-IDX DOWN BY 12
                       WHEN OTHER
                           SET EVO-IDX DOWN BY 13
                   END-EVALUATE
                   EVALUATE TRUE
                       WHEN D6 = 7 SET EVO-IDX DOWN BY 5
                       WHEN D6 = 8 SET EVO-IDX DOWN BY 10
                   END-EVALUATE
               WHEN D6 = 11 SET EVO-IDX TO 31
               WHEN D6 = 12 SET EVO-IDX TO 32
               WHEN D6 = 13 SET EVO-IDX TO 33
               WHEN OTHER SET EVO-IDX TO MAX-EVO
           END-EVALUATE
           MOVE EVO-IDX TO MASS-INDEX(STAR-IDX)
      D    DISPLAY '  MASS-INDEX 'MASS-INDEX(STAR-IDX)
           COMPUTE WS-TMP-NUM1 = EVO-MASS(MASS-INDEX(STAR-IDX))
           MOVE 0.05 TO WS-TMP-NUM0
           CALL 'ALTER-VALUE-BY-UPTO'  USING
                                       WS-TMP-NUM0, WS-TMP-NUM1,
                                       WS-TMP-NUM2
           COMPUTE MASS(STAR-IDX) ROUNDED = WS-TMP-NUM2
      D    DISPLAY '  MASS 'MASS(STAR-IDX)
           EXIT PARAGRAPH.

      *********************************
      * Some life stages require re-tuning of the star's initial mass
      * value. Lets get to it...
      *
       REFINE-STELLAR-MASS.
           EVALUATE TRUE
               WHEN CLASS-D(STAR-IDX)
                   COMPUTE MASS(STAR-IDX) ROUNDED =
                           FUNCTION RANDOM * 0.5 + 0.9
                   DISPLAY 're-MASS 'MASS(STAR-IDX)
               WHEN CLASS-X(STAR-IDX)
      D            DISPLAY ' -- black hole mass... yeah, mass.'
                   MOVE ERR-BH-MASS TO RETURN-CODE
           END-EVALUATE
           EXIT PARAGRAPH.

      *********************************
      * Determine number of stars in the system.
      *
       DETERMINE-NUM-STARS.
           CALL '3D6' USING D6
           IF IN-CLUSTER-OR-CORE THEN COMPUTE D6 = D6 + 3
           EVALUATE TRUE
               WHEN D6 <= 10 MOVE 1 TO WS-NUM-STARS
               WHEN D6 <= 15 MOVE 2 TO WS-NUM-STARS
               WHEN OTHER    MOVE 3 TO WS-NUM-STARS
           END-EVALUATE
           EXIT PARAGRAPH.

      *********************************
      * Determine avg. surface temperature in K.
      *
       DETERMINE-TEMPERATURE.
      D    DISPLAY '[determine-temperature] ' no ADVANCING
           EVALUATE TRUE
               WHEN CLASS-X(STAR-IDX)
      *            Black hole... What -is- their temperature?
      *            Infinite? Absolute zero or less?
      D            DISPLAY '-- black hole temperature undeterminable!'
                   MOVE ERR-BH-TEMP TO RETURN-CODE
               WHEN CLASS-D(STAR-IDX)
      *            According to Wiki, white dwarfs range from 3,000K
      *            up to 150,000K (or so).
                   COMPUTE WS-TMP-NUM1 =
                           FUNCTION RANDOM * 147000.0 + 3000.0          3-150KK
               WHEN CLASS-V(STAR-IDX) OR CLASS-VI(STAR-IDX)
                   MOVE EVO-AVG-TEMP(MASS-INDEX(STAR-IDX))
                        TO WS-TMP-NUM1
               WHEN CLASS-IV(STAR-IDX)
                   MOVE EVO-AVG-TEMP(MASS-INDEX(STAR-IDX))
                        TO WS-TMP-NUM1
                   COMPUTE WS-TMP-NUM1 =
                           WS-TMP-NUM1 -
                           ((WS-STELLAR-AGE -
                             EVO-M-SPAN(MASS-INDEX(STAR-IDX))) /
                             EVO-S-SPAN(MASS-INDEX(STAR-IDX)) *
                            (WS-TMP-NUM1 - 4800.0)
                           )
               WHEN OTHER
      *            Giant stars in general: 3-5,000K
                   COMPUTE WS-TMP-NUM1 =
                           FUNCTION RANDOM * 2000.0 + 3000.0
           END-EVALUATE
           CALL 'ALTER-VALUE-BY-UPTO'  USING
                                       K100, WS-TMP-NUM1, WS-TMP-NUM2
           COMPUTE TEMPERATURE(STAR-IDX) ROUNDED = WS-TMP-NUM2
      D    DISPLAY '  K = 'TEMPERATURE(STAR-IDX)'K'
           EXIT PARAGRAPH.

      *********************************
      * Determine a star's radius (in AU).
      *
       DETERMINE-RADIUS.
           EVALUATE TRUE
               WHEN CLASS-D(STAR-IDX)
                   MOVE 0.0 TO RADIUS(STAR-IDX)
           END-EVALUATE
           EXIT PARAGRAPH.
