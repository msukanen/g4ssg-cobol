       IDENTIFICATION DIVISION.
       PROGRAM-ID. GAS-GIANT-ARRANGEMENT.
       AUTHOR.     Markku Sukanen.
      ******************************************************************
      *
      * Determine arrangement (or lack of) gas giant(s) in the system.
      *
      ******************************************************************
       DATA DIVISION.
       LINKAGE SECTION.
       01  GG-ARRANGEMENT              PIC XX.
      *                                    NO - no gas giant
      *                                    CO - conventional gg
      *                                    EC - eccentric gg
      *                                    ES - epistellar gg

       PROCEDURE DIVISION USING GG-ARRANGEMENT.
           GOBACK.
