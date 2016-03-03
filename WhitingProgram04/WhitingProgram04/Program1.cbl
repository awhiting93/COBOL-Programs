**********************************************************************
*                                                                    *
* I PLEDGE THAT ALL OF THIS PROGRAM IS MY ORIGINAL WORK, AND THAT    *
* NONE OF THE PROGRAM HAS BEEN COPIED FROM ANYWHERE OR ANYONE UNLESS *
* I WAS SPECIFICALLY AUTHORIZED TO DO SO BY MY COBOL INSTRUCTOR.     *
*                                                                    *
*                                                                    *
*                      SIGNED: ____________________________________  *
*                                           (signature)              *
*                                                                    *
**********************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM04.
       AUTHOR. Whiting.
       DATE-WRITTEN. FEBRUARY 6, 2015.
      *This program is copied from the textbook page 102.
      *This program calculates discount amounts for customers.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT CUSTOMER-TRANS
               ASSIGN TO "..\..\..\..\CUSTOMER-TRANS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CUSTOMER-MASTER
               ASSIGN TO "..\..\..\..\CUSTOMER-MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-TRANS.
       01  CUSTOMER-REC.
           05  INDENT-IN             PIC X(5).
           05  SALES-IN              PIC 999V99.
           
       FD  CUSTOMER-MASTER.
       01  CUSTOMER-MASTER-REC.
           05  INDENT-OUT            PIC X(5).
           05  SALES-AMT-OUT         PIC 999V99.
           05  DISCOUNT-PERCENT-OUT  PIC V99.
           05  NET-OUT               PIC 999V99.
       
       WORKING-STORAGE SECTION.
       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS       PIC X(3)    VALUE  'YES'.
           
       01  PROGRAM-WORK-AREAS.
           05  WS-DISCOUNT-AMT              PIC 999V99.
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT  CUSTOMER-TRANS
                OUTPUT CUSTOMER-MASTER
           MOVE SPACES TO CUSTOMER-MASTER-REC
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ CUSTOMER-TRANS
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-PROCESS-DATA
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-TRANS
                 CUSTOMER-MASTER
           STOP RUN.
           
       200-PROCESS-DATA.
           MOVE INDENT-IN TO INDENT-OUT
           MOVE SALES-IN TO SALES-AMT-OUT
           IF   SALES-IN > 100.00
               MOVE .03 TO DISCOUNT-PERCENT-OUT
           ELSE
               MOVE .02 TO DISCOUNT-PERCENT-OUT
           END-IF
           MULTIPLY SALES-IN BY DISCOUNT-PERCENT-OUT 
               GIVING WS-DISCOUNT-AMT
           SUBTRACT WS-DISCOUNT-AMT FROM SALES-IN
               GIVING NET-OUT
           WRITE CUSTOMER-MASTER-REC.
       END PROGRAM PROGRAM04.