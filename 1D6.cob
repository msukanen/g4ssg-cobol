       IDENTIFICATION DIVISION.
       PROGRAM-ID. 1D6.
       AUTHOR.     Markku Sukanen

       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-RET-RND      PIC 9(5).

      ******************************************************************
      * Roll LK-NUM-DICE count of d6'es, sum the result and return.
      *
      * LK-SEED is used only if we absolutely need to repeat the same
      * pseudo random bunch.
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-RET-RND.
           COMPUTE LK-RET-RND = LK-RET-RND + (FUNCTION RANDOM * 6) + 1
           GOBACK.
