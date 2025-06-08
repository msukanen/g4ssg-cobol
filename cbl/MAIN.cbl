       IDENTIFICATION DIVISION.
       PROGRAM-ID.   G4SSGCRE.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 3, 2025
      ******************************************************************
      *
      * Star system generator, based on "GURPS 4e Space" rules.
      *
      ******************************************************************
       COPY        TESTENV.
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
           88  IN-CLUSTER-OR-CORE      VALUE 'Y'.
           88  NOT-IN-CLUSTER-OR-CORE  VALUE 'N'.
      *********************************
      * Stellar CSV related:
       77  MAX-EVO                     PIC 99 VALUE 34.                 CONSTANT
       77  MAX-MASSIVE-EVO             PIC 99 VALUE 10.                 CONSTANT
       01  WS-EVO-COUNT                USAGE COMP-1 VALUE 0.
       01  WS-EVO-M-COUNT              USAGE COMP-1 VALUE 0.
       01  WS-EVO-CSV.
           05  WS-CSV-MASS             PIC X(10).
           05  WS-CSV-APPROX-TYPE      PIC X(10).
           05  WS-CSV-AVG-TEMP         PIC X(10).
           05  WS-CSV-L-MIN            PIC X(10).
           05  WS-CSV-L-MAX            PIC X(10).
           05  WS-CSV-M-SPAN           PIC X(10).
           05  WS-CSV-S-SPAN           PIC X(10).
           05  WS-CSV-G-SPAN           PIC X(10).
       01  WS-EVO-CSV-MASSIVE.
           05  WS-CSV-MASS             PIC X(10).
           05  WS-CSV-LUM              PIC X(12).
           05  WS-CSV-AVG-TEMP         PIC X(10).
           05  WS-CSV-S-SPAN           PIC X(12).
       01  STELLAR-EVO
                   OCCURS 0 TO 100 TIMES                                ^MAXIMUM
                   DEPENDING ON WS-EVO-COUNT
                   INDEXED BY EVO-IDX.
           05  MASS                    USAGE COMP-2.
           05  APPROX-TYPE             PIC X(3).
           05  AVG-TEMP                USAGE COMP-1.
           05  LUMINOSITY-MIN          USAGE COMP-2.
           05  LUMINOSITY-MAX          USAGE COMP-2.
           05  SPAN-M                  USAGE COMP-2.
           05  SPAN-S                  USAGE COMP-2.
           05  SPAN-G                  USAGE COMP-2.
       01  STELLAR-EVO-MASSIVE
                   OCCURS 0 TO 100 TIMES                                ^MAXIMUM
                   DEPENDING ON WS-EVO-M-COUNT
                   INDEXED BY EVO-MASSIVE-IDX.
           05  MASS                    USAGE COMP-2.
           05  LUMINOSITY              USAGE COMP-2.
           05  AVG-TEMP                USAGE COMP-2.
           05  SPAN-S                  USAGE COMP-2.

       LINKAGE SECTION.
       01  LK-PARM.
           05  LK-P-LEN                PIC ZZ9.
           05  LK-P-DATA               PIC X(100).

       PROCEDURE DIVISION USING LK-PARM.
      *    ____________
      ****[    MAIN    ]****
      *   ^~~~~~~~~~~~~^
      *    Parse "command line":
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
           
      *    Parse our stellar CSV...:
           OPEN INPUT CSV-FILE
           SET EVO-IDX TO 0
           SET EVO-MASSIVE-IDX TO 0
           DISPLAY 'Processing CSV: ' NO ADVANCING
           PERFORM UNTIL EXIT
               READ CSV-FILE INTO CSV-LINE
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       PERFORM PARSE-CSV-LINE
               END-READ

               IF WAS-CSV-ERROR THEN
                   DISPLAY X"0a"'           CSV: ERROR'
                           X"0a"'           with 'CSV-LINE
                   GO TO BYE-BYE
               END-IF
           END-PERFORM
           DISPLAY X"0a"'           CSV: OK'.
       
       BYE-BYE.
           CLOSE CSV-FILE
      *_________________
      ****[END MAIN]****
           GOBACK.

      *********************************
      * Parse a line of CSV.
      *
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

       PARSE-CSV-LINE-M.
      *    Parse massive star stuff.
           UNSTRING CSV-LINE DELIMITED BY ',' INTO
                   WS-TMP-STR
                   WS-CSV-MASS OF WS-EVO-CSV-MASSIVE
                   WS-CSV-LUM
                   WS-CSV-AVG-TEMP OF WS-EVO-CSV-MASSIVE
                   WS-CSV-S-SPAN OF WS-EVO-CSV-MASSIVE
                   ON OVERFLOW
                       SET WAS-CSV-ERROR TO TRUE
                       EXIT PARAGRAPH.
           ADD 1 TO WS-EVO-M-COUNT
           EXIT PARAGRAPH.

       PARSE-CSV-LINE-N.
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
                       EXIT PARAGRAPH.
           ADD 1 TO WS-EVO-COUNT
           EXIT PARAGRAPH.
