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
       PROGRAM-ID. PROGRAM13.
       AUTHOR. Whiting.
       DATE-WRITTEN. APRIL 27, 2015.
      *This program performs sequential file updates on a customer
      *master file.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT OLD-MASTER-FILE
               ASSIGN TO "..\..\..\..\..\CUSTOMER-MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRANSACTION-FILE
               ASSIGN TO "..\..\..\..\..\CUSTOMER-TRANS.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT NEW-MASTER-FILE
               ASSIGN TO "..\..\..\..\..\NEW-CUSTOMER-MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT BAD-TRANSACTION-REPORT
               ASSIGN TO "..\..\..\..\..\WhitingBAD-REPORT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT GOOD-TRANSACTION-REPORT
               ASSIGN TO "..\..\..\..\..\WhitingGOOD-REPORT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  OLD-MASTER-FILE.
       01  OLD-MASTER-REC.
           05  OLD-CUSTOMER-ID             PIC XXX.
           05  OLD-CUSTOMER-NAME           PIC X(27).
           05  OLD-MAX-CREDIT              PIC 9(5)      USAGE IS PACKED-DECIMAL.
           05  OLD-CREDIT-CODE             PIC XX.
           05  OLD-BALANCE                 PIC S9(5)V99  USAGE IS PACKED-DECIMAL.
           
       FD  TRANSACTION-FILE.
       01  TRANSACTION-REC.
           05  TRANS-CUSTOMER-ID           PIC XXX.
           05  TRANS-CUSTOMER-NAME         PIC X(27).
           05  TRANS-MAX-CREDIT            PIC 9(5).
           05  TRANS-CREDIT-CODE           PIC XX.
           05  TRANS-BALANCE               PIC S9(5)V99.
           05  TRANS-TYPE-CODE             PIC X.
               88  IS-ADD                              VALUE 'A'.
               88  IS-CHANGE                           VALUES ARE '1', '2', '3'.
               88  IS-DELETE                           VALUE 'D'.
               88  IS-TOTAL-REPLACE                    VALUE '1'.
               88  IS-ADD-NUMERIC                      VALUE '2'.
               88  IS-SUBTRACT-NUMERIC                 VALUE '3'.
           
       FD  NEW-MASTER-FILE.
       01  NEW-MASTER-REC.
           05  NEW-CUSTOMER-ID             PIC XXX.
           05  NEW-CUSTOMER-NAME           PIC X(27).
           05  NEW-MAX-CREDIT              PIC 9(5)      USAGE IS PACKED-DECIMAL.
           05  NEW-CREDIT-CODE             PIC XX.
           05  NEW-BALANCE                 PIC S9(5)V99  USAGE IS PACKED-DECIMAL.
       
       FD  BAD-TRANSACTION-REPORT.
       01  BAD-TRANSACTION-REC             PIC X(80).
       
       FD  GOOD-TRANSACTION-REPORT.
       01  GOOD-TRANSACTION-REC            PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS      PIC X(3)    VALUE  'YES'.
           05  ERROR-FOUND                 PIC X       VALUE  'N'.

       01  PROGRAM-CONSTANTS.
           05  DASH                        PIC X       VALUE "-".
           05  DATE-SEP-CONST              PIC X(2)    VALUE ", ".
           05  TIME-SEP-CONST              PIC X       VALUE ":".
           05  AM-CONST                    PIC X(2)    VALUE "AM".
           05  PM-CONST                    PIC X(2)    VALUE "PM".
           05  LOWER-ALPHA                 PIC X(26)
               VALUE "abcdefghijklmnopqrstuvwxyz".
           05  UPPER-ALPHA                 PIC X(26)
               VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           05  CHECK-DIGIT-DIVISOR         PIC 99      VALUE 11.
           05  MAX-MAX-CREDIT              PIC 9(5)    VALUE 60000.

       01  PROGRAM-MESSAGES.
           05                              PIC X(57)
               VALUE "THE INPUT FILE IS EMPTY - THERE ARE NO RECORDS TO PROCESS".
           05  EQUAL-TO-ADD-ERR            PIC X(60)
               VALUE "CANNOT ADD CUSTOMER - A CUSTOMER WITH THIS ID ALREADY EXISTS".
           05  EQUAL-TO-CHANGE-ERR         PIC X(80)
               VALUE "CANNOT CHANGE RECORD INFORMATION".
           05  GREATER-THAN-CHANGE-ERR     PIC X(80)
               VALUE "CANNOT CHANGE RECORD INFORMATION - NO RECORD WITH THIS ID EXISTS".
           05  GREATER-THAN-DELETE-ERR     PIC X(80)
               VALUE "CANNOT DELETE RECORD - NO RECORD WITH THIS ID EXISTS".
           05  SUCCESSFUL-CHANGE-MSG       PIC X(17)
               VALUE "SUCCESSFUL CHANGE".
           05  SUCCESSFUL-DELETE-MSG       PIC X(17)
               VALUE "SUCCESSFUL DELETE".
           05  SUCCESSFUL-ADD-MSG          PIC X(14)
               VALUE "SUCCESSFUL ADD".
               
       01  PRINTER-CONTROL.
           05  PROPER-SPACING              PIC 9       VALUE 1.
           05  SPACE-ONE-LINE              PIC 9       VALUE 1.
           05  SPACE-TWO-LINES             PIC 9       VALUE 2.
           05  SPACE-THREE-LINES           PIC 9       VALUE 3.
           05  PAGE-COUNT                  PIC 9(3)    VALUE 1. 
           05  GOOD-LINES-PRINTED          PIC 9(2)    VALUE 99.
               88  GOOD-END-OF-THE-PAGE                VALUE 45 THRU 99.
           05  BAD-LINES-PRINTED           PIC 9(2)    VALUE 99.
               88  BAD-END-OF-THE-PAGE                 VALUE 45 THRU 99.
           05  TOP-MARGIN                  PIC 9       VALUE 6.

       01  PROGRAM-DATE-AND-TIME.
           05  DATE-TIME-WORK.
               10  YEAR-WORK               PIC 9(4).
               10  MONTH-WORK              PIC 9(2).
               10  DAY-WORK                PIC 9(2).
               10  HOURS-WORK              PIC 9(2).
                   88  IT-IS-MIDNIGHT                  VALUES ARE 00, 24. 
                   88  IT-IS-MORNING                   VALUES ARE 01 THRU 11.                    
                   88  IT-IS-NOON                      VALUE IS 12.
                   88  IT-IS-AFTER-NOON                VALUE 13 THRU 23.
               10  MINUTES-WORK            PIC 9(2).
               10  SECONDS-WORK            PIC 9(2).
               10  HUNDREDS-WORK           PIC 9(2).
           05  AM-PM-WORK                  PIC X(2).

      * 01  PROGRAM-ACCUMULATORS.  
       
       01  PROGRAM-COUNTERS.
           05  LINE-COUNT                  PIC 9(5)    VALUE ZERO.
           
      * 01  PROGRAM-COMPARE-AREAS.
       01  PROGRAM-WORK-AREAS.
           05  MAX-CREDIT                  PIC 9(5)    VALUE ZERO.
           05  BALANCE                     PIC 9(5)V99 VALUE ZERO.
               
      * 01  PROGRAM-SUBSCRIPTS.
           
       01  PROGRAM-TABLES.
           05  MONTHS-TABLE.
               10  MONTH-CONSTANTS.
                   15                      PIC X(9)    VALUE "JANUARY  ".
                   15                      PIC X(9)    VALUE "FEBRUARY ".
                   15                      PIC X(9)    VALUE "MARCH    ".
                   15                      PIC X(9)    VALUE "APRIL    ".
                   15                      PIC X(9)    VALUE "MAY      ".
                   15                      PIC X(9)    VALUE "JUNE     ".
                   15                      PIC X(9)    VALUE "JULY     ".
                   15                      PIC X(9)    VALUE "AUGUST   ".
                   15                      PIC X(9)    VALUE "SEPTEMBER".
                   15                      PIC X(9)    VALUE "OCTOBER  ".
                   15                      PIC X(9)    VALUE "NOVEMBER ".
                   15                      PIC X(9)    VALUE "DECEMBER ".
               10  MONTH-TABLE REDEFINES MONTH-CONSTANTS
                                           PIC X(9)    OCCURS 12 TIMES.
       01  PROGRAM-HEADING-LINES.
           05  HEADING-LINE-1.
               10  DATE-AND-TIME-HEADING.
                   15  DATE-HEADING        PIC X(18)   VALUE SPACES.
                   15                      PIC X(2)    VALUE SPACES.
                   15  TIME-HEADING        PIC X(7)    VALUE SPACES.
               10                          PIC X(5)    VALUE SPACES.
               10                          PIC X(15)   VALUE "CUSTOMER Report".
               10                          PIC X(9)    VALUE SPACES.
               10                          PIC X(12)   VALUE "  Whiting   ".
               10                          PIC X(2)    VALUE SPACES.
               10  PAGE-HEADING.
                   15                      PIC X(5)    VALUE "PAGE ".
                   15  PAGE-NUMBER-HEADING PIC ZZ9.
               10                          PIC X(2)    VALUE SPACES.
           05  HEADING-LINE-2.
               10                          PIC X(9)    VALUE "  ID #   ".
               10                          PIC X(22)   VALUE "CUSTOMER NAME".
               10                          PIC X(9)    VALUE SPACES.
               10                          PIC X(10)   VALUE "MAX CREDIT".
               10                          PIC XXX     VALUE SPACES.
               10                          PIC X(13)   VALUE "CREDIT STATUS".
               10                          PIC X(6)    VALUE SPACES.
               10                          PIC X(7)    VALUE "BALANCE".
               

       01  PROGRAM-REPORT-LINE.
           05                              PIC XX      VALUE SPACES.
           05  REPORT-CUSTOMER-ID          PIC XXX.
           05                              PIC X(4)    VALUE SPACES.
           05  REPORT-CUSTOMER-NAME        PIC X(27).
           05                              PIC X(4)    VALUE SPACES.
           05  REPORT-MAX-CREDIT           PIC $Z(4)9.99.
           05                              PIC X(10)   VALUE SPACES.
           05  REPORT-CREDIT-CODE          PIC XX.
           05                              PIC X(10)   VALUE SPACES.
           05  REPORT-BALANCE              PIC $Z(4)9.99.
           05                              PIC XX      VALUE SPACES.
                      
      * 01  PROGRAM-TOTAL-LINES.

       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           PERFORM 800-INITIALIZATION-ROUTINE
           PERFORM 110-DATE-TIME-ROUTINE
           PERFORM 115-GOOD-HEADING-INIT
           PERFORM 116-BAD-HEADING-INIT
           PERFORM 200-PROCESS-DATA
               UNTIL OLD-CUSTOMER-ID = HIGH-VALUES AND
                     TRANS-CUSTOMER-ID = HIGH-VALUES
           PERFORM 900-ENDING-ROUTINE
           PERFORM 999-UPDATE-MASTER
           STOP RUN.
       
       110-DATE-TIME-ROUTINE.
           MOVE FUNCTION CURRENT-DATE TO DATE-TIME-WORK
           STRING MONTH-TABLE (MONTH-WORK) DELIMITED BY SPACES
                  SPACE DELIMITED BY SIZE
                  DAY-WORK DELIMITED BY SIZE
                  DATE-SEP-CONST DELIMITED BY SIZE
                  YEAR-WORK DELIMITED BY SIZE
                      INTO DATE-HEADING
           EVALUATE TRUE
               WHEN IT-IS-AFTER-NOON
                   MOVE PM-CONST TO AM-PM-WORK
                   SUBTRACT 12 FROM HOURS-WORK
               WHEN IT-IS-MORNING
                   MOVE AM-CONST TO AM-PM-WORK
               WHEN IT-IS-NOON
                   MOVE PM-CONST TO AM-PM-WORK
               WHEN IT-IS-MIDNIGHT
                   MOVE AM-CONST TO AM-PM-WORK
                   MOVE 12 TO HOURS-WORK
               WHEN OTHER
                   DISPLAY "******* ERROR IN TIME *******"
           END-EVALUATE
           STRING HOURS-WORK DELIMITED BY SIZE
                  TIME-SEP-CONST DELIMITED BY SIZE
                  MINUTES-WORK DELIMITED BY SIZE
                  AM-PM-WORK DELIMITED BY SIZE
                      INTO TIME-HEADING.
           MOVE PAGE-COUNT TO PAGE-NUMBER-HEADING.
           
       115-GOOD-HEADING-INIT.
           MOVE ZERO TO GOOD-LINES-PRINTED
           WRITE GOOD-TRANSACTION-REC FROM HEADING-LINE-1 
               AFTER ADVANCING 1 LINE
           WRITE GOOD-TRANSACTION-REC FROM HEADING-LINE-2
               AFTER ADVANCING 2 LINES
           MOVE SPACES TO GOOD-TRANSACTION-REC
           WRITE GOOD-TRANSACTION-REC.
           
       116-BAD-HEADING-INIT.
           MOVE ZERO TO BAD-LINES-PRINTED
           WRITE BAD-TRANSACTION-REC FROM HEADING-LINE-1 
               AFTER ADVANCING 1 LINE
           WRITE BAD-TRANSACTION-REC FROM HEADING-LINE-2
               AFTER ADVANCING 2 LINES
           MOVE SPACES TO BAD-TRANSACTION-REC
           WRITE BAD-TRANSACTION-REC.
           
       120-HEADING-ROUTINE.
           MOVE SPACES TO GOOD-TRANSACTION-REC
           WRITE GOOD-TRANSACTION-REC
               AFTER ADVANCING PAGE.
           ADD 1 TO PAGE-COUNT
           MOVE PAGE-COUNT TO PAGE-NUMBER-HEADING
           WRITE GOOD-TRANSACTION-REC FROM HEADING-LINE-1
               AFTER ADVANCING 2 LINES
           WRITE GOOD-TRANSACTION-REC FROM HEADING-LINE-2
               AFTER ADVANCING 2 LINES
           MOVE SPACES TO GOOD-TRANSACTION-REC
           WRITE GOOD-TRANSACTION-REC
           MOVE ZERO TO GOOD-LINES-PRINTED.
           
       121-BAD-HEADING-ROUTINE.
           MOVE SPACES TO BAD-TRANSACTION-REC
           WRITE BAD-TRANSACTION-REC
               AFTER ADVANCING PAGE.
           ADD 1 TO PAGE-COUNT
           MOVE PAGE-COUNT TO PAGE-NUMBER-HEADING
           WRITE BAD-TRANSACTION-REC FROM HEADING-LINE-1
               AFTER ADVANCING 2 LINES
           WRITE BAD-TRANSACTION-REC FROM HEADING-LINE-2
               AFTER ADVANCING 2 LINES
           MOVE SPACES TO BAD-TRANSACTION-REC
           WRITE BAD-TRANSACTION-REC
           MOVE ZERO TO BAD-LINES-PRINTED.
           
       200-PROCESS-DATA.
           IF GOOD-END-OF-THE-PAGE
               PERFORM 120-HEADING-ROUTINE
           END-IF
           IF BAD-END-OF-THE-PAGE
               PERFORM 121-BAD-HEADING-ROUTINE
           END-IF
       
           EVALUATE TRUE
               WHEN OLD-CUSTOMER-ID < TRANS-CUSTOMER-ID
                    PERFORM 300-IGNORE-TRANSACTION-ROUTINE
               WHEN OLD-CUSTOMER-ID = TRANS-CUSTOMER-ID
                    PERFORM 310-CHECK-EQUAL-TO-CODE
               WHEN OLD-CUSTOMER-ID > TRANS-CUSTOMER-ID
                    PERFORM 320-CHECK-GREATER-THAN-CODE
           END-EVALUATE.
           
       300-IGNORE-TRANSACTION-ROUTINE.
           MOVE OLD-MASTER-REC TO NEW-MASTER-REC
           WRITE NEW-MASTER-REC
           PERFORM 700-READ-OLD-MASTER.
           
       310-CHECK-EQUAL-TO-CODE.
           EVALUATE TRUE
               WHEN IS-ADD
                    PERFORM 500-EQUAL-TO-ADD-ROUTINE
               WHEN IS-CHANGE
                    PERFORM 510-EQUAL-TO-CHANGE-ROUTINE
               WHEN IS-DELETE
                    PERFORM 520-EQUAL-TO-DELETE-ROUTINE
           END-EVALUATE.
           
       320-CHECK-GREATER-THAN-CODE.
           EVALUATE TRUE
               WHEN IS-ADD
                    PERFORM 600-GREATER-THAN-ADD-ROUTINE
               WHEN IS-CHANGE
                    PERFORM 610-GREATER-THAN-CHANGE-ROUTINE
               WHEN IS-DELETE
                    PERFORM 620-GREATER-THAN-DELETE-ROUTINE
           END-EVALUATE.
           
       500-EQUAL-TO-ADD-ROUTINE.
           PERFORM 650-MOVE-TO-REPORT
           WRITE BAD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
           WRITE BAD-TRANSACTION-REC FROM EQUAL-TO-ADD-ERR
           ADD 2 TO BAD-LINES-PRINTED
           PERFORM 710-READ-TRANSACTION.
           
       510-EQUAL-TO-CHANGE-ROUTINE.
           EVALUATE TRUE
               WHEN IS-TOTAL-REPLACE
                   PERFORM 515-TOTAL-REPLACE-ROUTINE
               WHEN IS-ADD-NUMERIC
                   PERFORM 516-ADD-NUMERIC-ROUTINE
               WHEN IS-SUBTRACT-NUMERIC
                   PERFORM 517-SUBTRACT-NUMERIC-ROUTINE
           END-EVALUATE
           PERFORM 700-READ-OLD-MASTER
           PERFORM 710-READ-TRANSACTION.
       
       515-TOTAL-REPLACE-ROUTINE.
           MOVE 'N' TO ERROR-FOUND
           PERFORM 650-MOVE-TO-REPORT
           PERFORM 652-MOVE-OLD-MASTER-TO-NEW-MASTER
           PERFORM 518-VALIDATE-NONNUMERIC-FIELDS
           
           IF(TRANS-MAX-CREDIT = SPACES)
               MOVE OLD-MAX-CREDIT TO NEW-MAX-CREDIT
               MOVE OLD-MAX-CREDIT TO REPORT-MAX-CREDIT
           ELSE
               MOVE TRANS-MAX-CREDIT TO NEW-MAX-CREDIT
               MOVE TRANS-MAX-CREDIT TO REPORT-MAX-CREDIT
           END-IF
           
           IF(TRANS-BALANCE IS NOT NUMERIC)
               MOVE OLD-BALANCE TO NEW-BALANCE
               MOVE OLD-BALANCE TO REPORT-BALANCE
           ELSE
               MOVE TRANS-BALANCE TO NEW-BALANCE
               MOVE TRANS-BALANCE TO REPORT-BALANCE
           END-IF
           IF(ERROR-FOUND = 'Y')
               WRITE BAD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
               WRITE BAD-TRANSACTION-REC FROM EQUAL-TO-CHANGE-ERR
               ADD 2 TO BAD-LINES-PRINTED
           ELSE
               WRITE GOOD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
               WRITE GOOD-TRANSACTION-REC FROM SUCCESSFUL-CHANGE-MSG
               ADD 2 TO GOOD-LINES-PRINTED
           END-IF
           WRITE NEW-MASTER-REC.
       
       516-ADD-NUMERIC-ROUTINE.
           MOVE 'N' TO ERROR-FOUND
           PERFORM 650-MOVE-TO-REPORT
           PERFORM 652-MOVE-OLD-MASTER-TO-NEW-MASTER
           PERFORM 518-VALIDATE-NONNUMERIC-FIELDS
           
           IF(TRANS-MAX-CREDIT IS NUMERIC AND
              TRANS-MAX-CREDIT NOT EQUAL ZERO)
               COMPUTE MAX-CREDIT = TRANS-MAX-CREDIT + OLD-MAX-CREDIT
               IF(MAX-CREDIT <= MAX-MAX-CREDIT AND
                  MAX-CREDIT >= ZERO)
                   MOVE MAX-CREDIT TO NEW-MAX-CREDIT
                   MOVE MAX-CREDIT TO REPORT-MAX-CREDIT
               ELSE
                   MOVE OLD-MAX-CREDIT TO NEW-MAX-CREDIT
                   MOVE OLD-MAX-CREDIT TO REPORT-MAX-CREDIT
                   MOVE "UNSUCCESSFUL CHANGE - MAX CREDIT MUST BE FROM $0 - $60,000"
                     TO EQUAL-TO-CHANGE-ERR
                   MOVE 'Y' TO ERROR-FOUND
               END-IF
           ELSE
               MOVE OLD-MAX-CREDIT TO MAX-CREDIT
               MOVE OLD-MAX-CREDIT TO NEW-MAX-CREDIT
               MOVE OLD-MAX-CREDIT TO REPORT-MAX-CREDIT
           END-IF
           IF(TRANS-BALANCE IS NUMERIC AND
              TRANS-BALANCE NOT EQUAL ZERO)
               COMPUTE BALANCE = TRANS-BALANCE + OLD-BALANCE
               IF(BALANCE <= MAX-CREDIT AND
                  BALANCE >= ZERO)
                  MOVE BALANCE TO NEW-BALANCE
                  MOVE BALANCE TO REPORT-BALANCE
               ELSE
                  MOVE OLD-BALANCE TO NEW-BALANCE
                  MOVE OLD-BALANCE TO REPORT-BALANCE
                  MOVE "UNSUCCESSFUL CHANGE - BALANCE MUST BE BETWEEN $0 AND THE MAXIMUM CREDIT ALLOWED"
                     TO EQUAL-TO-CHANGE-ERR
                  MOVE 'Y' TO ERROR-FOUND
               END-IF
           ELSE
               MOVE OLD-BALANCE TO NEW-BALANCE
               MOVE OLD-BALANCE TO REPORT-BALANCE
           END-IF
           IF(ERROR-FOUND = 'Y')
               WRITE BAD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
               WRITE BAD-TRANSACTION-REC FROM EQUAL-TO-CHANGE-ERR
               ADD 2 TO BAD-LINES-PRINTED
           ELSE
               WRITE GOOD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
               WRITE GOOD-TRANSACTION-REC FROM SUCCESSFUL-CHANGE-MSG
               ADD 2 TO GOOD-LINES-PRINTED
           END-IF
           MOVE ZERO TO MAX-CREDIT
           MOVE ZERO TO BALANCE
           WRITE NEW-MASTER-REC.    
       
       517-SUBTRACT-NUMERIC-ROUTINE.
           MOVE 'N' TO ERROR-FOUND
           PERFORM 650-MOVE-TO-REPORT
           PERFORM 652-MOVE-OLD-MASTER-TO-NEW-MASTER
           PERFORM 518-VALIDATE-NONNUMERIC-FIELDS
           
           IF(TRANS-MAX-CREDIT IS NUMERIC AND
              TRANS-MAX-CREDIT NOT EQUAL ZERO)
               COMPUTE MAX-CREDIT = OLD-MAX-CREDIT - TRANS-MAX-CREDIT
               IF(MAX-CREDIT <= MAX-MAX-CREDIT AND 
                  MAX-CREDIT >= ZERO)
                   MOVE MAX-CREDIT TO NEW-MAX-CREDIT
                   MOVE MAX-CREDIT TO REPORT-MAX-CREDIT
               ELSE
                   MOVE OLD-MAX-CREDIT TO NEW-MAX-CREDIT
                   MOVE OLD-MAX-CREDIT TO REPORT-MAX-CREDIT
                   MOVE "UNSUCCESSFUL CHANGE - MAX-CREDIT MUST BE FROM $0 - $60,000"
                     TO EQUAL-TO-CHANGE-ERR
                   MOVE 'Y' TO ERROR-FOUND
               END-IF
           ELSE
               MOVE OLD-MAX-CREDIT TO MAX-CREDIT
               MOVE OLD-MAX-CREDIT TO NEW-MAX-CREDIT
               MOVE OLD-MAX-CREDIT TO REPORT-MAX-CREDIT
           END-IF
           IF(TRANS-BALANCE IS NUMERIC AND
              TRANS-BALANCE NOT EQUAL ZERO)
               COMPUTE BALANCE = OLD-BALANCE - TRANS-BALANCE
               IF(BALANCE <= MAX-CREDIT AND
                  BALANCE >= ZERO)
                  MOVE BALANCE TO NEW-BALANCE
                  MOVE BALANCE TO REPORT-BALANCE
               ELSE
                  MOVE OLD-BALANCE TO NEW-BALANCE
                  MOVE OLD-BALANCE TO REPORT-BALANCE
                  MOVE "UNSUCCESSFUL CHANGE - BALANCE MUST BE BETWEEN $0 AND THE MAXIMUM CREDIT ALLOWED"
                     TO EQUAL-TO-CHANGE-ERR
                  MOVE 'Y' TO ERROR-FOUND
               END-IF
           ELSE
               MOVE OLD-BALANCE TO NEW-BALANCE
               MOVE OLD-BALANCE TO REPORT-BALANCE
           END-IF
           IF(ERROR-FOUND = 'Y')
               WRITE BAD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
               WRITE BAD-TRANSACTION-REC FROM EQUAL-TO-CHANGE-ERR
               ADD 2 TO BAD-LINES-PRINTED
           ELSE
               WRITE GOOD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
               WRITE GOOD-TRANSACTION-REC FROM SUCCESSFUL-CHANGE-MSG
               ADD 2 TO GOOD-LINES-PRINTED
           END-IF
           MOVE ZERO TO MAX-CREDIT
           MOVE ZERO TO BALANCE
           WRITE NEW-MASTER-REC.
       
       518-VALIDATE-NONNUMERIC-FIELDS.
           IF(TRANS-CUSTOMER-NAME = SPACES)
               MOVE OLD-CUSTOMER-NAME TO REPORT-CUSTOMER-NAME
               MOVE OLD-CUSTOMER-NAME TO NEW-CUSTOMER-NAME
           ELSE
               MOVE TRANS-CUSTOMER-NAME TO NEW-CUSTOMER-NAME
               MOVE TRANS-CUSTOMER-NAME TO REPORT-CUSTOMER-NAME
           END-IF
           IF(TRANS-CREDIT-CODE = SPACES)
               MOVE OLD-CREDIT-CODE TO REPORT-CREDIT-CODE
               MOVE OLD-CREDIT-CODE TO NEW-CREDIT-CODE
           ELSE
               MOVE TRANS-CREDIT-CODE TO NEW-CREDIT-CODE
               MOVE TRANS-CREDIT-CODE TO REPORT-CREDIT-CODE
           END-IF.    
       
       520-EQUAL-TO-DELETE-ROUTINE.
           PERFORM 651-MOVE-TO-REPORT-FOR-DELETE
           WRITE GOOD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
           WRITE GOOD-TRANSACTION-REC FROM SUCCESSFUL-DELETE-MSG
           ADD 2 TO GOOD-LINES-PRINTED
           PERFORM 710-READ-TRANSACTION.
           
       600-GREATER-THAN-ADD-ROUTINE.
           WRITE NEW-MASTER-REC FROM TRANSACTION-REC
           PERFORM 650-MOVE-TO-REPORT
           WRITE GOOD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
           WRITE GOOD-TRANSACTION-REC FROM SUCCESSFUL-ADD-MSG
           ADD 2 TO GOOD-LINES-PRINTED
           PERFORM 710-READ-TRANSACTION.
       
       610-GREATER-THAN-CHANGE-ROUTINE.
           PERFORM 650-MOVE-TO-REPORT
           WRITE BAD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
           WRITE BAD-TRANSACTION-REC FROM GREATER-THAN-CHANGE-ERR
           ADD 2 TO BAD-LINES-PRINTED
           PERFORM 710-READ-TRANSACTION.
       
       620-GREATER-THAN-DELETE-ROUTINE.
           PERFORM 650-MOVE-TO-REPORT
           WRITE BAD-TRANSACTION-REC FROM PROGRAM-REPORT-LINE
           WRITE BAD-TRANSACTION-REC FROM GREATER-THAN-DELETE-ERR
           ADD 2 TO BAD-LINES-PRINTED
           PERFORM 710-READ-TRANSACTION.
           
       650-MOVE-TO-REPORT.
           MOVE TRANS-CUSTOMER-ID TO REPORT-CUSTOMER-ID
           MOVE TRANS-CUSTOMER-NAME TO REPORT-CUSTOMER-NAME
           MOVE TRANS-MAX-CREDIT TO REPORT-MAX-CREDIT
           MOVE TRANS-CREDIT-CODE TO REPORT-CREDIT-CODE
           MOVE TRANS-BALANCE TO REPORT-BALANCE.
           
       651-MOVE-TO-REPORT-FOR-DELETE.
           MOVE OLD-CUSTOMER-ID TO REPORT-CUSTOMER-ID
           MOVE OLD-CUSTOMER-NAME TO REPORT-CUSTOMER-NAME
           MOVE OLD-MAX-CREDIT TO REPORT-MAX-CREDIT
           MOVE OLD-CREDIT-CODE TO REPORT-CREDIT-CODE
           MOVE OLD-BALANCE TO REPORT-BALANCE.
           
       652-MOVE-OLD-MASTER-TO-NEW-MASTER.
           MOVE OLD-CUSTOMER-ID TO NEW-CUSTOMER-ID
           MOVE OLD-CUSTOMER-NAME TO NEW-CUSTOMER-NAME
           MOVE OLD-MAX-CREDIT TO NEW-MAX-CREDIT
           MOVE OLD-CREDIT-CODE TO NEW-CREDIT-CODE
           MOVE OLD-BALANCE TO NEW-BALANCE.
           
       700-READ-OLD-MASTER.
           READ OLD-MASTER-FILE
               AT END MOVE HIGH-VALUES TO OLD-CUSTOMER-ID
           END-READ.
       
       710-READ-TRANSACTION.
           READ TRANSACTION-FILE
               AT END MOVE HIGH-VALUES TO TRANS-CUSTOMER-ID
           END-READ.
           
       800-INITIALIZATION-ROUTINE.
           OPEN INPUT  OLD-MASTER-FILE
                       TRANSACTION-FILE
                OUTPUT NEW-MASTER-FILE
                       BAD-TRANSACTION-REPORT
                       GOOD-TRANSACTION-REPORT.
           PERFORM 700-READ-OLD-MASTER
           PERFORM 710-READ-TRANSACTION.
           
       900-ENDING-ROUTINE.
           CLOSE OLD-MASTER-FILE
                 TRANSACTION-FILE
                 NEW-MASTER-FILE
                 BAD-TRANSACTION-REPORT
                 GOOD-TRANSACTION-REPORT.
                 
       999-UPDATE-MASTER.
           OPEN INPUT NEW-MASTER-FILE
               OUTPUT OLD-MASTER-FILE
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ NEW-MASTER-FILE
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       WRITE OLD-MASTER-REC FROM NEW-MASTER-REC
               END-READ
           END-PERFORM.
           CLOSE NEW-MASTER-FILE
                 OLD-MASTER-FILE.
                 
       END PROGRAM PROGRAM13.