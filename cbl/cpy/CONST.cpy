      ******************************************************************
      *
      * Various global "constants".
      *
      * Infinite (but not really) life-span.
       77  INF-LIFESPAN                PIC 9(5)V9 VALUE 99999.9.        CONSTANT
      * Generic - something-not-available - value.
       77  NOT-AVAILABLE               PIC 9V9 VALUE 0.0.               CONSTANT
      *********************************
      * For e.g. ALTER-VALUE-#
      * 
       77  FIVE-PERCENT                USAGE COMP-2 VALUE  5.0.         CONSTANT
       77  TEN-PERCENT                 USAGE COMP-2 VALUE 10.0.         CONSTANT
       
       77  K100                        USAGE COMP-2 VALUE 100.0.        CONSTANT
      *********************************
      * Some error codes:
      *
      * TODO: Black hole temperature - how to figure out?
       77  ERR-BH-TEMP                 PIC 999 VALUE 333.               CONSTANT
      * TODO: Black hole mass - how much should it differ from main-seq?
       77  ERR-BH-MASS                 PIC 999 VALUE 334.               CONSTANT
