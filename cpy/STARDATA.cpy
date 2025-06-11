      ******************************************************************
      * Data pertaining to a single star (or other shiny (or not so
      * shiny) celestial object).
      *
               10  MASS-INDEX          INDEX.                           ^EVO-IDX
               10  MASS                USAGE COMP-2.                    × Sol
               10  MASS-STAGE          PIC X VALUE '-'.
                   88  MASSIVE-STAR    VALUE 'M'
                                       WHEN SET TO FALSE IS '-'.
               10  LUMINOSITY          USAGE COMP-2.                    × Sol
               10  TEMPERATURE         USAGE COMP-2.                    Kelvin
               10  STAGE               PIC XXX.
                   COPY STARSTG.
               10  RADIUS              USAGE COMP-2.                    AU
