       IDENTIFICATION DIVISION.
       PROGRAM-ID. DICER000.
       AUTHOR.     Markku Sukanen

       DATA DIVISION.
      
       WORKING-STORAGE SECTION.
       
       01  LOCAL-SEED      PIC 9(4) comp-5.

       LINKAGE SECTION.
       
       01  LK-SEED         PIC 9(4) COMP-5.
       01  LK-RET-RND      USAGE IS COMP-1.

       PROCEDURE DIVISION USING LK-SEED, LK-RET-RND.
           MOVE LK-SEED TO LOCAL-SEED.
           IF LOCAL-SEED = 0 THEN
               COMPUTE LK-RET-RND = FUNCTION RANDOM
           ELSE
               COMPUTE LK-RET-RND = FUNCTION RANDOM(LOCAL-SEED)
           END-IF.
           GOBACK.
