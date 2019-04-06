      *******************************************************************
      ** Virtual printer subprogram
      *******************************************************************
       IDENTIFICATION DIVISION.
      **************************************
       PROGRAM-ID. VIRTUAL-PRINTER.
      **
       ENVIRONMENT DIVISION.
      ***************************************
      **
       INPUT-OUTPUT SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       FILE-CONTROL.
           SELECT FPRINTER ASSIGN to "./printer.dat"
           ORGANIZATION LINE SEQUENTIAL
       ACCESS SEQUENTIAL.
      **
       DATA DIVISION.
      **************************************
       FILE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       FD FPRINTER.
       01 ENREG-PRINTER PIC X(80).
      **
       WORKING-STORAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       LINKAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 RECEIVED-PARAM.
           02 PA-RESET         PIC X       .
           02 PA-BUFFER        PIC X(80)   .
           02 PA-WHEN          PIC X(6)    .
           02 PA-WHAT          PIC X(5)    .
           02 PA-HOWMANY       PIC 99      .
       PROCEDURE DIVISION
            USING
                RECEIVED-PARAM.
      **************************************
       MAIN-PRINTER.
           IF(PA-RESET = "O")
               OPEN OUTPUT FPRINTER
           ELSE
               OPEN EXTEND FPRINTER
               IF(PA-WHEN = "AFTER")
                   IF(PA-WHAT = "PAGE")
                       MOVE '>------------------------------------------'
      -'------------------------------------<' TO ENREG-PRINTER
                       WRITE ENREG-PRINTER
                   ELSE
                       SUBTRACT 1 FROM PA-HOWMANY
                       PERFORM PA-HOWMANY TIMES
                           MOVE SPACES TO ENREG-PRINTER
                           WRITE ENREG-PRINTER
                       END-PERFORM
                    END-IF
                END-IF
                WRITE ENREG-PRINTER FROM PA-BUFFER
                IF(PA-WHEN = "BEFORE")
                   IF(PA-WHAT = "PAGE")
                       MOVE '>------------------------------------------'
      -'------------------------------------<' TO ENREG-PRINTER
                       WRITE ENREG-PRINTER
                   ELSE
                       SUBTRACT 1 FROM PA-HOWMANY
                       PERFORM PA-HOWMANY TIMES
                           MOVE SPACES TO ENREG-PRINTER
                           WRITE ENREG-PRINTER
                       END-PERFORM
                   END-IF
               END-IF
           END-IF
           CLOSE FPRINTER
           MOVE "N"        TO PA-RESET
           MOVE SPACES     TO PA-BUFFER
           MOVE "AFTER"    TO PA-WHEN
           MOVE "LINES"    TO PA-WHAT
           MOVE 1          TO PA-HOWMANY
           EXIT PROGRAM.
       END PROGRAM VIRTUAL-PRINTER.
