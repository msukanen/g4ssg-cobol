       IDENTIFICATION DIVISION.
       PROGRAM-ID.   DETERMINE-ORBITAL-SEP.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 10, 2025
      ******************************************************************
      *
      * Determine orbital separation between stars in a multi-star
      * system.
      *
      * The third star in a trinary tends to be much further away than
      * the system's secondary is.
      *
      * NOTE:  STAR-COUNT  -- this is actually ~IGNORED~ but has to be
      *                       present for indexing/memory purposes.
      * TODO:              Change code so that STAR-COUNT can be left
      *                    out from confusing everyone.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY    RNG.
       LINKAGE SECTION.
       01  LK-THIRD-IN-TRINARY         PIC X VALUE '-'.
           88  IS-THIRD                VALUE 'Y'.
       01  STAR-COUNT                  PIC 999 USAGE COMP-5.            STARSEP!
       01  LK-SEP.
           COPY STARSEP.
       01  LK-EARLY-IDX                INDEX.
       01  LK-IDX                      INDEX.

       PROCEDURE DIVISION USING        LK-THIRD-IN-TRINARY, STAR-COUNT,
                                       LK-SEP, LK-EARLY-IDX, LK-IDX.
           IF LK-EARLY-IDX IS OMITTED THEN
               PERFORM DETERMINE-ORBITAL-SEP
           ELSE
               PERFORM UNTIL
                       SEPARATION(LK-IDX) >= SEPARATION(LK-EARLY-IDX)
                   PERFORM DETERMINE-ORBITAL-SEP
               END-PERFORM
           END-IF
           GOBACK.

       DETERMINE-ORBITAL-SEP.
           COPY 3D6.
           IF IS-THIRD THEN COMPUTE D6 = D6 + 6.
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 6
                   SET SEP-V-CLOSE(LK-IDX) TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 9
                   SET SEP-CLOSE(LK-IDX) TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 11
                   SET SEP-MODERATE(LK-IDX) TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 14
                   SET SEP-WIDE(LK-IDX) TO TRUE
               WHEN OTHER
                   SET SEP-DISTANT(LK-IDX) TO TRUE
           END-EVALUATE
           EXIT PARAGRAPH.
