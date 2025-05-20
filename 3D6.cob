       IDENTIFICATION DIVISION.
       PROGRAM-ID. 3D6.
       AUTHOR.     Markku Sukanen

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  RND1        PIC 9(5).
       01  RND2        PIC 9(5).
       01  RND3        PIC 9(5).
       LINKAGE SECTION.
       01  LK-RET-RND     PIC 9(5).

      ******************************************************************
      * Roll LK-NUM-DICE count of d6'es, sum the result and return.
      *
      * LK-SEED is used only if we absolutely need to repeat the same
      * pseudo random bunch.
      *
      ******************************************************************
       PROCEDURE DIVISION USING LK-RET-RND.
           CALL '1D6' USING RND1.
           CALL '1D6' USING RND2.
           CALL '1D6' USING RND3.
           INITIALIZE LK-RET-RND.
           COMPUTE LK-RET-RND = RND1 + RND2 + RND3.
           GOBACK.
