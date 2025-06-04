       IDENTIFICATION DIVISION.
       PROGRAM-ID.   EXPLAIN-GG-ARRANGEMENT.
       AUTHOR.       Markku Sukanen.
       DATE-WRITTEN. June 3, 2025
      ******************************************************************
      *
      * Explain/print GGA in more or less human readable form.
      *
      ******************************************************************
       DATA DIVISION.
       LINKAGE SECTION.
       01  LK-GGA                      PIC 9.
           COPY GGA.
       01  LK-XPGGA                    PIC X(79).

       PROCEDURE DIVISION USING LK-GGA, LK-XPGGA.
           EVALUATE TRUE
               WHEN NO-GAS-GIANT
                   MOVE 'no gas giant' TO LK-XPGGA
               WHEN CONVENTIONAL-GAS-GIANT
                   MOVE 'conventional gas giant' TO LK-XPGGA
               WHEN ECCENTRIC-GAS-GIANT
                   MOVE 'eccentric gas giant' TO LK-XPGGA
               WHEN EPISTELLAR-GAS-GIANT
                   MOVE 'epistellar gas giant' TO LK-XPGGA
               WHEN OTHER
      D            DISPLAY '[XPGGA.cbl]'
                   DISPLAY 'ERROR: developer brain malfunction detected'
                   DISPLAY 'Cause→ gas giant arrangement '
                           'out of bounds?'
                   MOVE 112 TO RETURN-CODE
                   STOP RUN
           END-EVALUATE
           GOBACK.
