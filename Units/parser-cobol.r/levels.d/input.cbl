       IDENTIFICATION DIVISION.
       PROGRAM-ID. Test-Items.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 RECORD1.
              05 ITEM1 PIC X(1).
              05 ITEM2 PIC A(1).
                     88 ODD VALUES 1, 3, 5, 7, 9.
                     88 EVEN VALUES 2, 4, 6, 8.
              05 ITEM3 PIC X(6).
              66 RDITEM4 RENAMES ITEM1 THRU ITEM2.
              66 RDITEM5 RENAMES ITEM1 THROUGH ITEM3.
       77 STDLN1 PIC A(4).
       01 REC2.
              02 G1.
                     05 ITEM1 PICTURE X(10).
                     05 ITEM2 PIC X(10).
              66 OTHERNAME1 RENAMES ITEM1 IN REC2.
              66 OTHERNAME2 RENAMES G1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Hello world".
            DISPLAY OTHERNAME2.
            STOP RUN.
       END PROGRAM Test-Items.
