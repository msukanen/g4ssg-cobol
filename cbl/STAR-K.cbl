       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-STAR-K.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 9, 2025.
      ******************************************************************p.104/
      *                                                                   126
      * Determine a star's temperature in Kelvin.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-A                        USAGE COMP-2.
       LINKAGE SECTION.
       01  LK-AGE.
           COPY STLRAGE.
       01  LK-EVO.
           COPY STLREVO.
       01  LK-STAR.
           COPY STARDATA.

       PROCEDURE DIVISION USING LK-EVO, LK-STAR.
           EVALUATE TRUE
               WHEN WHITE-DWARF                                         p.104
      *            White dwarves' surface temperature can vary wildly,
      *            from measly 3,050K to roasting 150,000K.
                   COMPUTE TEMPERATURE
                         = FUNCTION RANDOM * 146950 + 3050
               WHEN OTHER
                   IF MASSIVE-STAR OR CLASS-V OR CLASS-VI THEN          p.104/
                      MOVE AVG-TEMP TO TEMPERATURE                        126
                   ELSE PERFORM DET-STAR-K-N
           END-EVALUATE.
           GOBACK.
       
       DET-STAR-K-N.
           EVALUATE TRUE
               WHEN CLASS-IV                                            p.104
                   COMPUTE WS-A = BYR - SPAN-M
                   COMPUTE TEMPERATURE
                         = AVG-TEMP
                         - ((WS-A / SPAN-S) * (AVG-TEMP - 4800))
               WHEN OTHER
      *            III are rather "cool", from 3,000 to 5,000K.
                   COMPUTE TEMPERATURE = FUNCTION RANDOM * 2000 + 3000
           END-EVALUATE
           EXIT PARAGRAPH.
