       IDENTIFICATION DIVISION.
       PROGRAM-ID. 2D6.
       AUTHOR.     Markku Sukanen

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  RND1                        PIC 99 USAGE COMP-3.
       LINKAGE SECTION.
       01  LK-RET-RND                  PIC 99 USAGE COMP-3.

      ******************************************************************
      * Roll LK-NUM-DICE count of d6'es, sum the result and return.
      *
      * LK-SEED is used only if we absolutely need to repeat the same
      * pseudo random bunch.
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-RET-RND.
           CALL '1D6' USING RND1.
           CALL '1D6' USING LK-RET-RND.
           COMPUTE LK-RET-RND = LK-RET-RND + RND1.
           GOBACK.
