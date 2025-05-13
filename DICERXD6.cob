       IDENTIFICATION Division.
       Program-ID. DICERXD6.
       Author.     Markku Sukanen

       DATA Division.
       Working-Storage Section.
       01  local-seed      PIC S9(4) comp-5.

       Linkage Section.
       01  lk-seed         PIC S9(4) comp-5.
       01  lk-num-dice     PIC S9(1) comp-5.
       01  lk-ret-rnd      USAGE is comp-1.

       PROCEDURE Division using lk-seed, lk-num-dice, lk-ret-rnd.
       RND-0-TO-1.
           MOVE lk-seed to local-seed.
           if local-seed = 0 then
               COMPUTE lk-ret-rnd = function RANDOM
           else
               COMPUTE lk-ret-rnd = function RANDOM(local-seed)
           end-if.
           Goback.
