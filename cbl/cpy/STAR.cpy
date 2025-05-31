      *        Mass-index, as per 4eSpace mass, but as an index.
           05  MASS-INDEX              INDEX.
           05  MASS                    PIC 9(5)V9(5) USAGE COMP-3.       × Sol
           05  STAGE                   PIC X(4).
               COPY STARSTG.
           05  TEMPERATURE             PIC 9(5)V9 USAGE COMP-3.         Kelvins
           05  LUMINOSITY              PIC 9(12)V9 USAGE COMP-3.         × Sol
