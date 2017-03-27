       IDENTIFICATION DIVISION.
           EXEC TAA IDENTIFY EFUN SERVICE
                FAWEFUN-11 IN FAW
           END-EXEC.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
       01  WS-ABBRUCH-TYP          PIC 99.
           88  WS-ABBRUCH-EVENT    VALUE 91.
.          88  WS-ABBRUCH-OBJMGR   VALUE 92.
.          88  WS-ABBRUCH-DB       VALUE 93.
.          88  WS-ABBRUCH-PGM      VALUE 94.
       01  F-TEXT.
           05 F-TEXT-DB            PIC X(5).
           05 FILLER               PIC X VALUE SPACE.
           05 F-TEXT-KEY1          PIC 9(8).
           05 FILLER               PIC X VALUE '/'.
           05 F-TEXT-KEY2          PIC 9(8).
           05 FILLER               PIC X VALUE '/'.
           05 F-TEXT-KEY3          PIC 99.
           05 FILLER               PIC X(7) VALUE ' EVENT:'.
           05 F-TEXT-EVENT         PIC X(10).
      *
       COPY TCTOENV.
       COPY TCWFLENG.
       COPY TCPR.

       LINKAGE SECTION.
      *
       PROCEDURE DIVISION.
      *
       MAIN SECTION.
           EXEC TAA REGISTER END-EXEC


           EXEC TAA ON SEVERE EXIT ARBG END-EXEC


           PERFORM M01-VORLAUF


           EVALUATE TRUE
               WHEN TC-EVENT-PRUEFEN OF ME
                    PERFORM E01-PRUEFEN
                    SET TC-STATE-OK OF ME TO TRUE
               WHEN OTHER
                    SET WS-ABBRUCH-EVENT TO TRUE
                    PERFORM M99-ABBRUCH
           END-EVALUATE


           PERFORM M02-NACHLAUF


           EXEC TAA UNREGISTER END-EXEC
      *
           CONTINUE.
       MAIN-EX. EXIT.
      *
       E01-PRUEFEN SECTION.
           EXIT.
      *
       M01-VORLAUF SECTION.
           CONTINUE.
       M01-VORLAUF-EX.
           EXIT.
      *
       M02-NACHLAUF SECTION.
           CONTINUE.
       M02-NACHLAUF-EX.
           EXIT.
      *
       M99-ABBRUCH SECTION.
           EVALUATE TRUE
               WHEN WS-ABBRUCH-OBJMGR
                   EXEC TAA
                       SET AND RAISE SEVERE
                       GROUP OMERR
                       CODE 0
                   END-EXEC
               WHEN WS-ABBRUCH-EVENT
                   EXEC TAA
                       SET SEVERE
                       GROUP AAEZUG01
                       CODE 2
                       ARGUMENTS = (TC-EVENT)
                   END-EXEC
                   EXEC TAA
                       SET AND RAISE SEVERE
                       GROUP USERERR
                       CODE 1
                   END-EXEC
               WHEN WS-ABBRUCH-PGM
                   EXEC TAA
                       SET SEVERE
                       GROUP AAEZUG01
                       CODE 3
                       ARGUMENTS = (TX-IM-SHORTNAME)
                   END-EXEC
                   EXEC TAA
                       SET AND RAISE SEVERE
                       GROUP USERERR
                       CODE 1
                   END-EXEC
               WHEN OTHER
                   EXEC TAA
                       SET AND RAISE SEVERE
                       GROUP USERERR
                       CODE 0
                   END-EXEC
           END-EVALUATE
           CONTINUE.
       M99-ABBRUCH-EX. EXIT.
