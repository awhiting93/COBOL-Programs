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
       PROGRAM-ID. PROGRAM06.
       AUTHOR. Andrew Whiting.
       DATE-WRITTEN. FEBRUARY 23, 2015.
      *This program is derived from the textbook page 298.
      *This program creates a employee payroll report.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT PAYROLL-MASTER-FILE
               ASSIGN TO "..\..\..\..\..\PAYROLL-MASTER.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT PAYROLL-REPORT
               ASSIGN TO "..\..\..\..\..\WhitingPAYROLL-REPORT.LIS"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL-MASTER-FILE
           RECORD CONTAINS 80 CHARACTERS.
       01  PAYROLL-MASTER-REC.
           05  EMP-NUM               PIC X(5).
           05  EMP-NAME              PIC X(20).
           05                        PIC X(4).
           05  ANNUAL-SALARY         PIC 9(6).
           05                        PIC X(13).
           05  UNION-DUES            PIC 999V99.
           05  INSURANCE             PIC 999V99.
           05                        PIC X(22).
           
       FD  PAYROLL-REPORT.
       01  PAYROLL-REPORT-LINE      PIC X(82).
       
       WORKING-STORAGE SECTION.
       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS      PIC X(3)    VALUE  'YES'.

       01  PROGRAM-CONSTANTS.
           05  DASH                        PIC X       VALUE "-".
           05  DATE-SEP-CONST              PIC X(2)    VALUE ", ".
           05  TIME-SEP-CONST              PIC X       VALUE ":".
           05  AM-CONST                    PIC X(2)    VALUE "AM".
           05  PM-CONST                    PIC X(2)    VALUE "PM".
           05  SALARY-INCREASE             PIC V99     VALUE .07.
           05  UNION-DUES-INCREASE         PIC V99     VALUE .04.
           05  INSURANCE-INCREASE          PIC V99     VALUE .03.

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
               88  END-OF-THE-PAGE                     VALUE 54 THRU 99.
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
      * 01  PROGRAM-COUNTERS.
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
               10                          PIC X(20)   VALUE "    Payroll Report  ".
               10                          PIC X(4)    VALUE SPACES.
               10                          PIC X(12)   VALUE SPACES.
               10                          PIC X(5)    VALUE SPACES.
               10  PAGE-HEADING.
                   15                      PIC X(5)    VALUE "PAGE ".
                   15  PAGE-NUMBER-HEADING PIC ZZ9.
               10                          PIC X(2)    VALUE SPACES.
           05  HEADING-LINE-2.
               10  EMP-NUM-HEADING1        PIC X(14)   VALUE " EMPLOYEE".
               10                          PIC X(4)    VALUE SPACES.
               10  EMP-NAME-HEADING        PIC X(4)    VALUE "NAME".
               10                          PIC X(12)   VALUE SPACES.
               10  OLD-SALARY-HEADING1     PIC X(8)    VALUE "OLD     ".
               10  NEW-SALARY-HEADING1     PIC X(9)    VALUE "NEW      ".
               10  OLD-DUES-HEADING1       PIC X(8)    VALUE "OLD     ".
               10  NEW-DUES-HEADING1       PIC X(9)    VALUE "NEW      ".
               10  OLD-INSUR-HEADING1      PIC X(8)    VALUE "OLD     ".
               10  NEW-INSUR-HEADING1      PIC X(3)    VALUE "NEW".
           05  HEADING-LINE-3.
               10  EMP-NUM-HEADING2        PIC X(7)    VALUE "    NO.".
               10                          PIC X(25)   VALUE SPACES.
               10  OLD-SALARY-HEADING2     PIC X(9)    VALUE " SALARY  ".
               10  NEW-SALARY-HEADING2     PIC X(8)    VALUE "SALARY  ".
               10  OLD-DUES-HEADING2       PIC X(10)   VALUE "  DUES    ".
               10  NEW-DUES-HEADING2       PIC X(8)    VALUE "DUES    ".
               10  OLD-INSUR-HEADING2      PIC X(8)    VALUE "INSUR.  ".
               10  NEW-INSUR-HEADING1      PIC X(6)    VALUE "INSUR.".        
       
       01  PROGRAM-DETAIL-LINE.
           05                              PIC XXX     VALUE SPACES.
           05  EMP-NUM-OUT                 PIC X(5).
           05                              PIC XX      VALUE SPACES.
           05  EMP-NAME-OUT                PIC X(20).
           05                              PIC XX      VALUE SPACES.
           05  OLD-SALARY-OUT              PIC $Z(5)9.
           05                              PIC X       VALUE SPACES.
           05  NEW-SALARY-OUT              PIC $(6)9.
           05                              PIC XX      VALUE SPACES.
           05  OLD-DUES-OUT                PIC $ZZZ.99.
           05                              PIC X       VALUE SPACES.
           05  NEW-DUES-OUT                PIC $(4).99.
           05                              PIC XX      VALUE SPACES.
           05  OLD-INSUR-OUT               PIC $ZZZ.99.
           05                              PIC X       VALUE SPACES.
           05  NEW-INSUR-OUT               PIC $(4).99.

      * 01  PROGRAM-TOTAL-LINES.

       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT  PAYROLL-MASTER-FILE
                OUTPUT PAYROLL-REPORT
           PERFORM 110-DATE-TIME-ROUTINE
           WRITE PAYROLL-REPORT-LINE FROM HEADING-LINE-1 
               AFTER ADVANCING 5 LINES.
           WRITE PAYROLL-REPORT-LINE FROM HEADING-LINE-2 
               AFTER ADVANCING 2 LINES.
           WRITE PAYROLL-REPORT-LINE FROM HEADING-LINE-3 
               AFTER ADVANCING 1 LINE.
           MOVE  SPACES TO PAYROLL-REPORT-LINE.
           WRITE PAYROLL-REPORT-LINE 
               AFTER ADVANCING 1 LINE.
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ PAYROLL-MASTER-FILE
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-PROCESS-DATA
               END-READ
           END-PERFORM
           CLOSE PAYROLL-MASTER-FILE
                 PAYROLL-REPORT.
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
           
       200-PROCESS-DATA.
           MOVE EMP-NUM       TO EMP-NUM-OUT
           MOVE EMP-NAME      TO EMP-NAME-OUT
           MOVE ANNUAL-SALARY TO OLD-SALARY-OUT
           MOVE UNION-DUES    TO OLD-DUES-OUT
           MOVE INSURANCE     TO OLD-INSUR-OUT
           COMPUTE NEW-SALARY-OUT
               ROUNDED = (ANNUAL-SALARY + (ANNUAL-SALARY * SALARY-INCREASE))
           COMPUTE NEW-DUES-OUT
               ROUNDED = (UNION-DUES + (UNION-DUES * UNION-DUES-INCREASE))
           COMPUTE NEW-INSUR-OUT
               ROUNDED = (INSURANCE + (INSURANCE * INSURANCE-INCREASE))
           WRITE PAYROLL-REPORT-LINE FROM PROGRAM-DETAIL-LINE
       END PROGRAM PROGRAM06.