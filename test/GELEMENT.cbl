       IDENTIFICATION DIVISION.
       PROGRAM-ID.   TEST-GEN-ASTEROID-BELT.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 5, 2025.
       COPY          TESTENV.
      ******************************************************************
      *
      * Generate an asteroid belt.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM-OF-ORBITS.
           05  WS-NUM-BELTS            PIC 99 VALUE 0.
           05  WS-NUM-TERRS            PIC 99 VALUE 0.
           05  WS-NUM-GGS              PIC 99 VALUE 0.
           05  WS-NUM-EMPTY            PIC 99 VALUE 0.
           05  WS-NUM-ORBITS           PIC 99 VALUE 0.
       01  WS-LOOP                     PIC 99.
       01  WS-BELT OCCURS 0 TO 10 TIMES
                   DEPENDING ON WS-NUM-BELTS
                   INDEXED BY BELT-IDX.
           COPY EASTBELT.
       01  WS-TERR OCCURS 0 TO 10 TIMES
                   DEPENDING ON WS-NUM-TERRS
                   INDEXED BY TERR-IDX.
           COPY ETERR.
       01  WS-GG   OCCURS 0 TO 10 TIMES
                   DEPENDING ON WS-NUM-GGS
                   INDEXED BY GG-IDX.
           COPY EGG.
       01  WS-ORBIT OCCURS 0 TO 10 TIMES
                   DEPENDING ON WS-NUM-ORBITS
                   INDEXED BY ORBIT-IDX.
           10  ELEM-TYPE               PIC X.
               COPY ETYPE.
           10  ELEMENT                 INDEX.
       
       PROCEDURE DIVISION.
      * Belt #1
           ADD 1 TO WS-NUM-ORBITS GIVING WS-NUM-ORBITS
           SET ORBIT-IDX TO 1
           SET BELT-IDX TO 1
           SET ELEM-ABELT OF WS-ORBIT(ORBIT-IDX) TO TRUE
           SET ELEMENT OF WS-ORBIT(ORBIT-IDX) TO BELT-IDX
           CALL 'GEN-ASTEROID-BELT' USING WS-BELT(BELT-IDX)
      * Belt #2
           ADD 1 TO WS-NUM-ORBITS GIVING WS-NUM-ORBITS
           SET ORBIT-IDX UP BY 1
           SET BELT-IDX UP BY 1
           SET ELEM-ABELT OF WS-ORBIT(ORBIT-IDX) TO TRUE
           SET ELEMENT OF WS-ORBIT(ORBIT-IDX) TO BELT-IDX
           CALL 'GEN-ASTEROID-BELT' USING WS-BELT(BELT-IDX)
      * Terrestrial #3
           ADD 1 TO WS-NUM-ORBITS GIVING WS-NUM-ORBITS
           SET ORBIT-IDX UP BY 1
           SET TERR-IDX TO 1
           SET ELEM-TERRESTRIAL OF WS-ORBIT(ORBIT-IDX) TO TRUE
           SET ELEMENT OF WS-ORBIT(ORBIT-IDX) TO TERR-IDX
           CALL 'GEN-TERRESTRIAL' USING WS-TERR(TERR-IDX)

           PERFORM VARYING ORBIT-IDX FROM 1 BY 1
                   UNTIL ORBIT-IDX > WS-NUM-ORBITS
               DISPLAY 'orbit-idx 'ORBIT-IDX NO ADVANCING
      *        MOVE ELEM-TYPE(ORBIT-IDX) TO WS-ELEM-TYPE
               EVALUATE TRUE
                   WHEN ELEM-ABELT OF ELEM-TYPE(ORBIT-IDX)
                       DISPLAY ' B 'ELEMENT(ORBIT-IDX)
                   WHEN ELEM-TERRESTRIAL OF ELEM-TYPE (ORBIT-IDX)
                       DISPLAY ' T 'ELEMENT(ORBIT-IDX)
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       GEN-BELT.
           
           EXIT PARAGRAPH.
