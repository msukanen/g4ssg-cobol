      *    LK-RES-STR will contain the final result string.
      *    DISPLAY '  FROM: "' LK-SRC-NUM '"'
           MOVE LK-SRC-NUM TO WS-ED-STR
      *    DISPLAY ' Z♯.9♯: "' WS-ED-STR '"'
           INSPECT WS-ED-STR TALLYING WS-L FOR LEADING SPACE
      *    DISPLAY '  WS-L: ' WS-L
           MOVE FUNCTION REVERSE(WS-ED-STR) TO WS-FMT-STR
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-L
               IF WS-FMT-STR(WS-I:1) = '0' THEN
                   MOVE SPACE TO WS-FMT-STR(WS-I:1)
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM
      *    DISPLAY '    ←→: "' WS-FMT-STR '"'
           INITIALIZE LK-RES-STR
           STRING FUNCTION TRIM(WS-FMT-STR)
                  DELIMITED BY SIZE INTO LK-RES-STR
           MOVE FUNCTION REVERSE(LK-RES-STR) TO LK-RES-STR
      *    DISPLAY 'RESULT: "' FUNCTION TRIM(LK-RES-STR) '"'
