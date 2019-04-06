      *******************************************************************
      **=================================================================
      ** Ce programme teste le module VIRTUAL-PRINTER
      **
      ** Auteurs: Colin Duquesnoy, Thomas Bertels
      ** Date: Mai 2013
      *******************************************************************
       IDENTIFICATION DIVISION.
      **************************************
       PROGRAM-ID. TEST-PRINTER.
      **
       ENVIRONMENT DIVISION.
      **************************************
      **
       INPUT-OUTPUT SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
      **
       DATA DIVISION.
      **************************************
       FILE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
      **
       WORKING-STORAGE SECTION.
      **-*-*-*-*-*-*-*-*-*-*-*-*-*
       01 PRINTER-PARAM.
           02 PA-RESET         PIC X       VALUE "N"       .
           02 PA-BUFFER        PIC X(80)   VALUE SPACES    .
           02 PA-WHEN          PIC X(6)    VALUE "AFTER"   .
           02 PA-WHAT          PIC X(5)    VALUE "LINES"   .
           02 PA-HOWMANY       PIC 99      VALUE 1         .
       01 BUF-NB-LIGNES-PAR-PAGE PIC 99 VALUE 10.
      **
       PROCEDURE DIVISION.
      **************************************
       MAIN-PROCEDURE.
           DISPLAY "Proj UF31: Test virtual printer"

           DISPLAY "Combien de lignes par pages desirez-vous ?"
           ACCEPT BUF-NB-LIGNES-PAR-PAGE

           MOVE "O"        TO PA-RESET
           CALL "VIRTUAL-PRINTER"
                USING PRINTER-PARAM

           CALL "VIRTUAL-PRINTER2"
                USING PRINTER-PARAM
       END PROGRAM TEST-PRINTER.
