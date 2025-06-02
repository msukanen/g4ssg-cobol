      ******************************************************************
      *
      * Data pertaining to an individual star (or other at least equally
      * massive celestial object).
      *
      *        Mass-index, as per 4eSpace mass, but as an index.
           05  MASS-INDEX              INDEX.
           05  MASS                    PIC 9(5)V9(5) USAGE COMP-3.       × Sol
           05  STAGE                   PIC X(4).
               COPY STARSTG.
           05  TEMPERATURE             PIC 9(6)V9 USAGE COMP-3.          K
      *************
      * The brightest object in the universe (for now) would require
      * luminosity field for at least 15 integers + some decimal(s).
      *
      * See https://www.smithsonianmag.com/smart-news/astronomers-discover-the-brightest-known-object-in-the-universe-shining-500-trillion-times-as-bright-as-the-sun-180983815/
      *
           05  LUMINOSITY              PIC 9(15)V99 USAGE COMP-3.        × Sol
           05  RADIUS                  PIC 9(5)V9(5) USAGE COMP-3.       AU
           05  ORBITAL-ZONES.
               COPY ORBZONE.
           05  PRIMARY-GAS-GIANT.
               10  GG-DISTANCE         PIC 9(5)V9(5) USAGE COMP-3.       AU
               10  GG-ARRANGEMENT      PIC 9.
                   COPY GGA.
