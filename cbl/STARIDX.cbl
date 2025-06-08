       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GET-MASS-INDEX.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 8, 2025.
      ******************************************************************p.101
      *
      * Get mass based on mass-index.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY        RNG.
       01  WS-LOWER-BOUND              USAGE COMP-2.
       01  WS-UPPER-BOUND              USAGE COMP-2.
      * 0.51 overlaps slightly between values, indirectly favoring
      * earlier evo indexes.
       77  WS-MASS-TOLERANCE           USAGE COMP-2 VALUE 0.051.
       01  WS-LOOP                     PIC 999 USAGE COMP-5.

       LINKAGE SECTION.
       01  LK-SRCH-MASS                USAGE COMP-2.
       01  LK-EVO-REC.
           05  EVO-COUNT               PIC 999 USAGE COMP-5.
           05  EVO                     OCCURS 0 TO 100 TIMES
                                       DEPENDING ON EVO-COUNT
                                       INDEXED BY EVO-IDX.
               COPY STLREVO.
       01  LK-STAR.
           COPY STARDATA.

       PROCEDURE DIVISION USING LK-SRCH-MASS, LK-EVO-REC, LK-STAR.
      *    Floats are seldom exact and thus we use a tolerance value.
           COMPUTE WS-LOWER-BOUND = LK-SRCH-MASS - WS-MASS-TOLERANCE
           COMPUTE WS-UPPER-BOUND = LK-SRCH-MASS + WS-MASS-TOLERANCE

           SET EVO-IDX TO 1                                                !!
           SEARCH EVO
               AT END
      *            No match found, toss some error code.
      D            DISPLAY 'ERROR: mass-index not found for mass of '
      D                    LK-SRCH-MASS ' (mass out of bounds?)!'
                   SET EVO-IDX TO 0
                   MOVE 911 TO RETURN-CODE
               WHEN MASS OF EVO(EVO-IDX) > WS-LOWER-BOUND
                AND MASS OF EVO(EVO-IDX) < WS-UPPER-BOUND
                   MOVE EVO-IDX TO MASS-INDEX
           END-SEARCH
           GOBACK.
