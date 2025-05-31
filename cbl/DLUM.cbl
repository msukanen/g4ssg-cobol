       IDENTIFICATION DIVISION.
       PROGRAM-ID. DETERMINE-LUMINOSITY.
       AUTHOR.     Markku Sukanen.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY CONST.
       01  WS-LUM                      USAGE COMP-2.
       01  WS-TMP1                     USAGE COMP-2.
       COPY ALTBY10.

       LINKAGE SECTION.
       01  LK-EVO.
           COPY STLREVO.
       01  LK-STAGE                    PIC X(4).
           COPY STARSTG.
       01  LK-AGE                      PIC 99V99.
       01  LUMINOSITY                  PIC 9(12)V9(5) USAGE COMP-3.

       PROCEDURE DIVISION USING        LK-EVO, LK-STAGE, LK-AGE,
                                       LUMINOSITY.
           IF EVO-L-MAX IS EQUAL TO INF-LIFESPAN
               COMPUTE WS-LUM = EVO-L-MIN
           ELSE EVALUATE TRUE
               WHEN CLASS-V
                   COMPUTE WS-LUM =
                           EVO-L-MIN + (
                            (LK-AGE / EVO-M-SPAN) *
                            (EVO-L-MAX - EVO-L-MIN)
                           )
               WHEN CLASS-IV
                   COMPUTE WS-LUM = EVO-L-MAX
               WHEN  CLASS-III OR
                     CLASS-II OR
                     CLASS-IA OR
                     CLASS-IB
                   COMPUTE WS-LUM = 25.0 * EVO-L-MAX
               END-EVALUATE
           END-IF
      D    DISPLAY 'WS-LUM 'WS-LUM

           CALL 'ALTER-VALUE-BY-PERCENTAGE' USING
                                       TEN-PERCENT, WS-LUM, WS-TMP1
           COMPUTE LUMINOSITY ROUNDED = WS-TMP1
      D    DISPLAY '[luminosity]'
      D    DISPLAY '  sol× = 'LUMINOSITY
           GOBACK.
