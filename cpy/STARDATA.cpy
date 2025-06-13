      ******************************************************************
      * Data pertaining to a single star (or other shiny (or not so
      * shiny) celestial object).
      *
               10  MASS-INDEX          INDEX.                           ^EVO-IDX
               10  MASS                USAGE COMP-2.                    × Sol
      *            Initial mass is the mass the star had before it
      *            transformed into e.g. giant or a dwarf (or black
      *            hole, etc.).
               10  INITIAL-MASS        USAGE COMP-2.                    × Sol
               10  MASS-STAGE          PIC X VALUE '-'.
                   88  MASSIVE-STAR    VALUE 'M'
                                       WHEN SET TO FALSE IS '-'.
               10  LUMINOSITY.
                   15  CURRENT-LUM     USAGE COMP-2.                    × Sol
                   15  INITIAL-LUM     USAGE COMP-2.                    × Sol
               10  TEMPERATURE         USAGE COMP-2.                    Kelvin
               10  STAGE               PIC XXX.
                   COPY STARSTG.
               10  RADIUS              USAGE COMP-2.                    AU
               10  ORBIT-LIMITS.
                   15  INNER-LIMIT     USAGE COMP-2.                    AU
                   15  OUTER-LIMIT     USAGE COMP-2.                    AU
                   15  SNOW-LINE       USAGE COMP-2.                    AU
               10  FORBIDDEN-ZONE-COUNT PIC 99 VALUE 0.
               10  FORBIDDEN-ZONES     OCCURS 99 TIMES
                                       INDEXED BY FZ-IDX.
                   15  INNER-LIMIT     USAGE COMP-2.                    AU
                   15  OUTER-LIMIT     USAGE COMP-2.                    AU
