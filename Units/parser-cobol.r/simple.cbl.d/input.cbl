       IDENTIFICATION DIVISION.
       PROGRAM-ID. Program-Name.
       AUTHOR. Darren Hiebert.

       ENVIRONMENT DIVISION.
       INPUT-OUPUT SECTION.
       FILE-CONTROL.
           SELECT File-Name ASSIGN TO "SAMPLE.DAT"
                ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD File-Name.
       01 File-Group-Name.
         02 File-Data-Item          PIC 9(7).

       WORKING-STORAGE SECTION.
       01 Group-Name.
         02 Data-Item1              PIC 9 VALUE ZEROS.
           03 SH-WORK-MMDDYYYY      PIC  9(08)  VALUE 0.
           03 SH-WORK-MMDDYYYY-2    REDEFINES SH-WORK-MMDDYYYY.
           03  DW-DAYS-IN-MONTHS    VALUE "312831303130313130313031".
             05  DW-DAYS-IN-MONTH   OCCURS 12 TIMES
                                        PIC  9(02).

       PROCEDURE DIVISION.
       Begin.
           STOP RUN.

       Subprogram-Name.
