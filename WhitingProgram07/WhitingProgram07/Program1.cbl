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
       PROGRAM-ID. PROGRAM07.
       AUTHOR. Whiting.
       DATE-WRITTEN. FEBRUARY 27, 2015.
      *This program is derived from the textbook page 256.
      *This program creates a customer transaction report.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT CUSTOMER-FILE
               ASSIGN TO "..\..\..\..\..\CUSTOMER-FILE.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT CUSTOMER-REPORT
               ASSIGN TO "..\..\..\..\..\WhitingTRANS-REPORT.LIS"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CUSTOMER-FILE
           RECORD CONTAINS 24 CHARACTERS.
       01  CUST-TRANS-REC.
           05  INITIAL1-IN           PIC X.
           05  INITIAL2-IN           PIC X.
           05  LAST-NAME-IN          PIC X(10).
           05  TRANS-DATE-IN.
               10  TRANS-MONTH-IN    PIC XX.
               10  TRANS-YEAR-IN     PIC X(4).
           05  TRANS-AMT-IN          PIC 9(6).
           
       FD  CUSTOMER-REPORT.
       01  CUST-TRANS-REC-OUT        PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS      PIC X(3)    VALUE  'YES'.

       01  PROGRAM-CONSTANTS.
           05  DASH                        PIC X       VALUE "-".
           05  DATE-SEP-CONST              PIC X(2)    VALUE ", ".
           05  TIME-SEP-CONST              PIC X       VALUE ":".
           05  AM-CONST                    PIC X(2)    VALUE "AM".
           05  PM-CONST                    PIC X(2)    VALUE "PM".

       01  PROGRAM-MESSAGES.
           05                              PIC X(57)
               VALUE "THE INPUT FILE IS EMPTY - THERE ARE NO RECORDS TO PROCESS".

       01  PRINTER-CONTROL.
           05  PROPER-SPACING              PIC 9       VALUE 1.
           05  SPACE-ONE-LINE              PIC 9       VALUE 1.
           05  SPACE-TWO-LINES             PIC 9       VALUE 2.
           05  SPACE-THREE-LINES           PIC 9       VALUE 3.
           05  PAGE-COUNT                  PIC 9(3)    VALUE 1. 
           05  LINES-PRINTED               PIC 9(2)    VALUE 99.
               88  END-OF-THE-PAGE                     VALUE 45 THRU 99.
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

       01  PROGRAM-ACCUMULATORS.
           05  TOTAL                       PIC 9(20)V99
                                                       VALUE ZERO.
       01  PROGRAM-COUNTERS.
           05  LINE-COUNT                  PIC 9(5)    VALUE ZERO.
           
      * 01  PROGRAM-COMPARE-AREAS.
      * 01  PROGRAM-WORK-AREAS.
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
               10                          PIC X(21)   VALUE "Customer Transactions".
               10                          PIC X(4)    VALUE SPACES.
               10                          PIC X(12)   VALUE "  Whiting   ".
               10                          PIC X(2)    VALUE SPACES.
               10  PAGE-HEADING.
                   15                      PIC X(5)    VALUE "PAGE ".
                   15  PAGE-NUMBER-HEADING PIC ZZ9.
               10                          PIC X(2)    VALUE SPACES.
           05  HEADING-LINE-2.
               10  CUST-NAME               PIC X(14)   VALUE "    NAME      ".
               10  TRANS-DATE              PIC X(22)   VALUE "DATE OF TRANSACTION   ".
               10  TRANS-AMT               PIC X(21)   VALUE "AMOUNT OF TRANSACTION".

       01  PROGRAM-DETAIL-LINE.
           05  INITIAL1-OUT                PIC X.
           05  INITIAL-SEPARATOR           PIC X       VALUE ".".
           05  INITIAL2-OUT                PIC X.
           05  INITIAL-SEPARATOR           PIC X       VALUE ".".
           05  LAST-NAME-OUT               PIC X(10).
           05                              PIC X(6)    VALUE SPACES.
           05  TRANS-DATE-OUT.
               10  TRANS-MONTH-OUT         PIC XX.
               10  SLASH-SEPARATOR         PIC X       VALUE "/".
               10  TRANS-YEAR-OUT          PIC X(4).
           05                              PIC X(11)   VALUE SPACES.
           05  TRANS-AMT-OUT               PIC $ZZZ,ZZ9.

       01  PROGRAM-TOTAL-LINES.
           05                              PIC X(5)    VALUE "Total".
           05                              PIC X(23)   VALUE SPACES.
           05  TOTAL-OUT                   PIC $$$,$$$,$$$,$$9.99
                                                       VALUE ZERO.

       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT  CUSTOMER-FILE
                OUTPUT CUSTOMER-REPORT
           PERFORM 110-DATE-TIME-ROUTINE
           WRITE CUST-TRANS-REC-OUT FROM HEADING-LINE-1 
               AFTER ADVANCING 1 LINE.
           WRITE CUST-TRANS-REC-OUT FROM HEADING-LINE-2 
               AFTER ADVANCING 2 LINES.
           MOVE SPACES TO CUST-TRANS-REC-OUT
           WRITE CUST-TRANS-REC-OUT
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ CUSTOMER-FILE
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-PROCESS-DATA
               END-READ
           END-PERFORM
           MOVE TOTAL TO TOTAL-OUT
           IF LINE-COUNT >= 48
               PERFORM 120-HEADING-ROUTINE
           END-IF
           WRITE CUST-TRANS-REC-OUT FROM PROGRAM-TOTAL-LINES
                   AFTER ADVANCING 1 LINE
           CLOSE CUSTOMER-FILE
                 CUSTOMER-REPORT.
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
                      INTO TIME-HEADING
           MOVE PAGE-COUNT TO PAGE-NUMBER-HEADING.
           
       120-HEADING-ROUTINE.
           MOVE SPACES TO CUST-TRANS-REC-OUT
           WRITE CUST-TRANS-REC-OUT
               AFTER ADVANCING PAGE.
           ADD 1 TO PAGE-COUNT
           MOVE PAGE-COUNT TO PAGE-NUMBER-HEADING
           WRITE CUST-TRANS-REC-OUT FROM HEADING-LINE-1 
               AFTER ADVANCING 2 LINES.
           WRITE CUST-TRANS-REC-OUT FROM HEADING-LINE-2 
               AFTER ADVANCING 2 LINE.
           MOVE SPACES TO CUST-TRANS-REC-OUT
           WRITE CUST-TRANS-REC-OUT
           MOVE ZERO TO LINE-COUNT.
           
       200-PROCESS-DATA.
           ADD 1 TO LINE-COUNT
           IF LINE-COUNT >= 50
               PERFORM 120-HEADING-ROUTINE
           END-IF
           MOVE INITIAL1-IN    TO INITIAL1-OUT
           MOVE INITIAL2-IN    TO INITIAL2-OUT
           MOVE LAST-NAME-IN   TO LAST-NAME-OUT
           MOVE TRANS-MONTH-IN TO TRANS-MONTH-OUT
           MOVE TRANS-YEAR-IN  TO TRANS-YEAR-OUT
           MOVE TRANS-AMT-IN   TO TRANS-AMT-OUT
           COMPUTE TOTAL = TOTAL + TRANS-AMT-IN
           WRITE CUST-TRANS-REC-OUT FROM PROGRAM-DETAIL-LINE.
       END PROGRAM PROGRAM07.