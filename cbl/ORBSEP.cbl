       IDENTIFICATION DIVISION.
       PROGRAM-ID.   GENERATE-ORBITAL-SEP-CATEGORY.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 10, 2025
      ******************************************************************
      *
      * Determine orbital separation between stars in a multi-star
      * system.
      *
      * The third star in a trinary ~tends to be~ much further away than
      * e.g. the system's secondary.
      *
      ******************************************************************
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY    RNG.
       LINKAGE SECTION.
       01  LK-IS-THIRD                 PIC X.
           88  IS-THIRD                VALUE 'Y'.
       01  LK-SEP-CATEGORY             PIC XX.
           COPY SEPCATEG.

       PROCEDURE DIVISION USING LK-IS-THIRD, LK-SEP-CATEGORY.
           COPY 3D6.
           IF IS-THIRD THEN COMPUTE D6 = D6 + 6.
           EVALUATE TRUE
               WHEN D6 IS LESS OR EQUAL TO 6
                   SET SEP-V-CLOSE TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 9
                   SET SEP-CLOSE TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 11
                   SET SEP-MODERATE TO TRUE
               WHEN D6 IS LESS OR EQUAL TO 14
                   SET SEP-WIDE TO TRUE
              WHEN OTHER
                   SET SEP-DISTANT TO TRUE
           END-EVALUATE.
           EXIT PARAGRAPH.
