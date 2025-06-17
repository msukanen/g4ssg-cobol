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
               10  STAGE               PIC XXX.
                   COPY STARSTG.
               10  LUMINOSITY.
                   15  CURRENT-LUM     USAGE COMP-2.                    × Sol
      *                Initial luminosity value equals to CURRENT-LUM
      *                if the star's STAGE is that of a main sequence.
      *                For other stages it holds the value the star had
      *                back when it was still in its main sequence.
                   15  INITIAL-LUM     USAGE COMP-2.                    × Sol
               10  TEMPERATURE         USAGE COMP-2.                    Kelvin
               10  RADIUS              USAGE COMP-2.                    AU
      *            Inner-limit defines the distance below which any
      *            orbital element would either vaporize, not form, or
      *            would be sucked into the star.  Outer-limit defines
      *            the range beyond which any element would be too far
      *            out for the star's gravity to keep hold of it.  Snow
      *            line defines the range within which liquid water
      *            could be found, if present at all.
               10  ORBIT-LIMITS.
                   COPY ORBLIM.
      *************
      * TODO: Forbidden zones math somewhere.
      *
               10  FORBIDDEN-ZONE-COUNT PIC 99 VALUE 0.
               10  FORBIDDEN-ZONES     OCCURS 99 TIMES                    H0X!
                                       INDEXED BY FZ-IDX.
                   15  INNER-LIMIT     USAGE COMP-2.                    AU
                   15  OUTER-LIMIT     USAGE COMP-2.                    AU
      *************
      * Any given star's initial GG arrangement data:
      *
               10  GAS-GIANT-ARRANGEMENT.
                   15  ARRANGEMENT     PIC X VALUE '-'.
                       COPY GGARR.cpy.
                   15  DISTANCE        USAGE COMP-2.                    AU
      *************
      * Any given star's orbital elements live here, or rather,
      * references to them live here.
      *
      *            200 is (currently) the maximum number of orbits we
      *            support.  A star could, theoretically, have more?
      *            
               10  NUM-ORBITS          PIC 999 VALUE 0.
               10  ORBIT               OCCURS 200 TIMES                   H0X!
                                       INDEXED BY ORB-IDX,
                                                  DST-ORB-IDX,
                                                  PREV-ORB-IDX,
                                                  NEXT-ORB-IDX.
                   15  DISTANCE        USAGE COMP-2.                    AU
      *                Object's type, or '-' if nothing.
                   15  OBJ             PIC X VALUE '-'.
                       COPY ORBELEM.cpy.
      *                Object references live in this index.  The actual
      *                table that is used will depend on the above OBJ
      *                value.  Of course, if OBJ is '-', the OBJ-REF is
      *                most likely invalid.
                   15  OBJ-REF         INDEX.
