  ******************************************************************
       IDENTIFICATION DIVISION.
      *AUTHOR. Axel CHABAN.
       PROGRAM-ID. DBBTEST.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. MAINFRAME.
      *****************************************************************
      *** File Control                                              ***
      *****************************************************************
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
      ****************************************************************
      *** File Section                                              ***
      *****************************************************************
       FILE SECTION.
       WORKING-STORAGE SECTION.
****** 01 CUSTOMER-COPY.
******     COPY EPSMTCOM.
       01 DATE-JOUR PIC X(10).
       PROCEDURE DIVISION.
                DISPLAY 'AXELTESTDBB_N*1_04092023_VSCODE'.
                PERFORM A010-AFFICHAGE-DATE.
      *
       A010-AFFICHAGE-DATE.
                MOVE '2023-05-02' TO DATE-JOUR.
                STOP RUN.