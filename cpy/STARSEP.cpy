           05  STAR-SEP                OCCURS 0 TO 200 TIMES            Irrelevt
                                       DEPENDING ON STAR-COUNT          for 1st
                                       INDEXED BY SEP-IDX.              star.
               10  SEPARATION-DETAILS.
                   15  SEPARATION      PIC S9 VALUE -1.                 1st N/A
                     88  SEP-V-CLOSE     VALUE 1.
                     88  SEP-CLOSE       VALUE 2.
                     88  SEP-MODERATE    VALUE 3.
                     88  SEP-WIDE        VALUE 4.
                     88  SEP-DISTANT     VALUE 5.
                   15  ECCENTRICITY    USAGE COMP-2.
      *            Distance is the average distance to the primary star.
      *            Minimum/maximum distance are derived from this with
      *            help of the ECCENTRICITY of their separation.
                   15  DISTANCE        USAGE COMP-2.
