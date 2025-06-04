       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-LUMINOSITY.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 4, 2025
      ******************************************************************p.104
      *
      * Determine a star's luminosity.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CONST.
       01  WS-LUM                      USAGE COMP-2.
       01  WS-TMP1                     USAGE COMP-2.
       01  WS-AGE                      USAGE COMP-2.

       LINKAGE SECTION.
       01  LK-EVO.
           COPY STLREVO.
       01  LK-STAGE                    PIC X(4).
           COPY STARSTG.
       01  LK-AGE                      PIC 99V99.
       01  LUMINOSITY                  PIC 9(12)V9(5) USAGE COMP-3.

       PROCEDURE DIVISION USING        LK-EVO, LK-AGE, LK-STAGE,
                                       LUMINOSITY.
      D    DISPLAY '[determine-luminosity]'
           COMPUTE WS-AGE = LK-AGE
           IF EVO-L-MAX IS EQUAL TO INF-LIFESPAN
               COMPUTE WS-LUM = EVO-L-MIN
           ELSE EVALUATE TRUE
               WHEN CLASS-V OR CLASS-VI
                   COMPUTE WS-LUM =
                           EVO-L-MIN + (
                            (WS-AGE / EVO-M-SPAN) *
                            (EVO-L-MAX - EVO-L-MIN)
                           )
               WHEN CLASS-IV
                   COMPUTE WS-LUM = EVO-L-MAX
               WHEN CLASS-III OR
                     CLASS-II OR
                     CLASS-IA OR
                     CLASS-IB
                   COMPUTE WS-LUM = 25.0 * EVO-L-MAX
               WHEN CLASS-D
      *            0.0001 to ~0.001
                   COMPUTE WS-LUM = FUNCTION RANDOM * 0.001 + 0.0001
               WHEN OTHER
                   MOVE 0.0 TO WS-LUM
               END-EVALUATE
           END-IF
           CALL 'ALTER-VALUE-BY-PERCENTAGE'
                   USING TEN-PERCENT, WS-LUM, WS-TMP1
           COMPUTE LUMINOSITY ROUNDED = WS-TMP1
      D    DISPLAY '  sol× = 'LUMINOSITY
           GOBACK.
