       IDENTIFICATION DIVISION.
       PROGRAM-ID. DICENXD6.
       AUTHOR.     Markku Sukanen

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  TMP-VAL         PIC S9(5)V9(9).

       LINKAGE SECTION.

       01  LK-NUM-DICE     PIC S9(1) COMP-5.
       01  LK-RET-RND      PIC 9(5).

      ******************************************************************
      * Roll LK-NUM-DICE count of d6'es, sum the result and return.
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-NUM-DICE, LK-RET-RND.
      * Zero LK-RET-RND just in case.
           MOVE 0 TO LK-RET-RND.
           PERFORM LK-NUM-DICE TIMES
               COMPUTE LK-RET-RND = LK-RET-RND
                       + (6 * FUNCTION RANDOM)
                       + 1
           END-PERFORM.
           GOBACK.
