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
       PROGRAM-ID. PROGRAM12.
       AUTHOR. Whiting.
       DATE-WRITTEN. APRIL 15, 2015.
      *This program is derived from the textbook page 343.
      *This program assigns parking spots based off of a car's vin number.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
           SELECT VIN-FILE
               ASSIGN TO "..\..\..\..\..\VIN2.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT VIN-LETTER-FILE
               ASSIGN TO "..\..\..\..\..\VINLetters.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT VIN-WEIGHT-FILE
               ASSIGN TO "..\..\..\..\..\VINWeights.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT VIN-REPORT
               ASSIGN TO "..\..\..\..\..\WhitingVIN-REPORT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  VIN-FILE
           RECORD CONTAINS 42 CHARACTERS.
       01  VIN-REC.
           05  ID-NUMBER-IN                PIC X(5).
           05  NAME-IN                     PIC X(20).
           05  VIN-IN                      PIC X       OCCURS 17 TIMES.
           
       FD  VIN-LETTER-FILE.
       01  VIN-LETTER-RECORD.
           05  LETTER                      PIC X.
           05  NUMERIC-VALUE               PIC 9.
           
       FD  VIN-WEIGHT-FILE.
       01  VIN-WEIGHT-RECORD.
           05  VIN-WEIGHT-VALUE            PIC 99.
       
       FD  VIN-REPORT.
       01  VIN-REPORT-REC                  PIC X(80).
       
       WORKING-STORAGE SECTION.
       01  PROGRAM-INDICATORS.
           05  ARE-THERE-MORE-RECORDS      PIC X(3)    VALUE  'YES'.

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
           05  CONVERTED-VIN-ACCUMULATION  PIC 999     VALUE ZERO.
       01  PROGRAM-COUNTERS.
           05  LINE-COUNT                  PIC 9(5)    VALUE ZERO.
           
      * 01  PROGRAM-COMPARE-AREAS.
       01  PROGRAM-WORK-AREAS.
           05 YEAR-CHECK                   PIC X.
               88  IS-NEWER-CAR                        VALUES ARE 'Y', 'y', '1', '2', '3', '4', '5', '6', '7', '8', '9'.
           05  CONTINENT-CHECK             PIC X.
               88  IS-NORTH-AMERICA                    VALUES ARE '1', '2', '3', '4', '5'.
           05  CHECK-DIGIT                 PIC 99.
               88 CHECK-DIGIT-IS-VALID                 VALUES ARE 0 THRU 10.
           05  CALCULATED-CHECK-DIGIT      PIC 99.
           05  RESULT-OF-DIVISION          PIC 999.
               
       01  PROGRAM-SUBSCRIPTS.
           05  VIN-SUB                     PIC 99.
           05  CONTINENT-SUB               PIC 9       VALUE 1.
           05  YEAR-DIVISION-SUB           PIC 9       VALUE 7.
           05  CHECK-DIGIT-SUB             PIC 9       VALUE 9.
           05  YEAR-IDENTIFIER-SUB         PIC 99      VALUE 10.
           05  LETTER-SUB                  PIC 99.
           05  WEIGHT-SUB                  PIC 99.
           
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
           05  CONVERTED-VIN               PIC 9       OCCURS 17 TIMES.
           05  WS-LETTER-CONVERSION-PAIRS              OCCURS 23 TIMES INDEXED BY LETTER-INDEX.
               10  WS-LETTER               PIC X.
               10  WS-NUMERIC-VALUE        PIC 9.
           05  WS-VIN-WEIGHT-VALUE         PIC 99      OCCURS 17 TIMES.

       01  PROGRAM-HEADING-LINES.
           05  HEADING-LINE-1.
               10  DATE-AND-TIME-HEADING.
                   15  DATE-HEADING        PIC X(18)   VALUE SPACES.
                   15                      PIC X(2)    VALUE SPACES.
                   15  TIME-HEADING        PIC X(7)    VALUE SPACES.
               10                          PIC X(5)    VALUE SPACES.
               10                          PIC X(21)   VALUE "VIN Report".
               10                          PIC X(14)   VALUE SPACES.
               10                          PIC X(12)   VALUE "  Whiting   ".
               10                          PIC X(2)    VALUE SPACES.
               10  PAGE-HEADING.
                   15                      PIC X(5)    VALUE "PAGE ".
                   15  PAGE-NUMBER-HEADING PIC ZZ9.
               10                          PIC X(2)    VALUE SPACES.
           05  HEADING-LINE-2.
               10                          PIC X(14)   VALUE "  ID #        ".
               10                          PIC X(22)   VALUE "OWNER NAME           ".
               10                          PIC X(11)   VALUE "      VIN #".
               10                          PIC X(9)    VALUE SPACES.
               10                          PIC X(11)   VALUE "PARKING LOT".
               

       01  PROGRAM-DETAIL-LINE.
           05                              PIC XX      VALUE SPACES.
           05  ID-NUMBER-OUT               PIC X(5).
           05                              PIC X(4)    VALUE SPACES.
           05  NAME-OUT                    PIC X(20).
           05                              PIC X(4)    VALUE SPACES.
           05  VIN-OUT                     PIC X       OCCURS 17 TIMES.
           05                              PIC X(9)    VALUE SPACES.
           05  PARKING-STATUS              PIC X(5).
           05  CHECK-DIGIT-OUT             PIC 99.
           05                              PIC XX      VALUE SPACES.
           05  CALCULATED-CHECK-DIGIT-OUT  PIC 99.
           05                              PIC XX      VALUE SPACES.
           05  CONVERTED-VIN-ACCUM-OUT     PIC 999.
           
      * 01  PROGRAM-TOTAL-LINES.

       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT  VIN-FILE
                       VIN-LETTER-FILE
                       VIN-WEIGHT-FILE
                OUTPUT VIN-REPORT
           PERFORM 110-DATE-TIME-ROUTINE
           WRITE VIN-REPORT-REC FROM HEADING-LINE-1 
               AFTER ADVANCING 1 LINE.
           WRITE VIN-REPORT-REC FROM HEADING-LINE-2
               AFTER ADVANCING 2 LINES.
           MOVE SPACES TO VIN-REPORT-REC
           WRITE VIN-REPORT-REC
           PERFORM 250-LOAD-LETTER-TABLE
               VARYING LETTER-SUB FROM 1 BY 1
               UNTIL LETTER-SUB > 23
           PERFORM 260-LOAD-WEIGHT-TABLE
               VARYING WEIGHT-SUB FROM 1 BY 1
               UNTIL WEIGHT-SUB > 17
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ VIN-FILE
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-PROCESS-DATA
               END-READ
           END-PERFORM
           IF END-OF-THE-PAGE
               PERFORM 120-HEADING-ROUTINE
           END-IF
           CLOSE VIN-FILE
                 VIN-LETTER-FILE
                 VIN-WEIGHT-FILE
                 VIN-REPORT.
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
           
       120-HEADING-ROUTINE.
           MOVE SPACES TO VIN-REPORT-REC
           WRITE VIN-REPORT-REC
               AFTER ADVANCING PAGE.
           ADD 1 TO PAGE-COUNT
           MOVE PAGE-COUNT TO PAGE-NUMBER-HEADING
           WRITE VIN-REPORT-REC FROM HEADING-LINE-1
               AFTER ADVANCING 2 LINES.
           WRITE VIN-REPORT-REC FROM HEADING-LINE-2
               AFTER ADVANCING 2 LINES.
           MOVE SPACES TO VIN-REPORT-REC
           WRITE VIN-REPORT-REC
           MOVE ZERO TO LINES-PRINTED.
           
       200-PROCESS-DATA.
           ADD 1 TO LINES-PRINTED
           IF END-OF-THE-PAGE
               PERFORM 120-HEADING-ROUTINE
           END-IF
       
           MOVE ID-NUMBER-IN TO ID-NUMBER-OUT
           MOVE NAME-IN TO NAME-OUT
           PERFORM 300-MOVE-VIN
           PERFORM 320-CONVERT-TO-NUMERIC
           PERFORM 400-CALCULATE-CHECK-DIGIT
           PERFORM 310-CHECK-PARKING
           PERFORM VARYING VIN-SUB FROM 1 BY 1 
               UNTIL VIN-SUB > 17
               MOVE VIN-IN (VIN-SUB) TO VIN-OUT (VIN-SUB)
           END-PERFORM
           MOVE CHECK-DIGIT TO CHECK-DIGIT-OUT
           MOVE CALCULATED-CHECK-DIGIT TO CALCULATED-CHECK-DIGIT-OUT
           MOVE CONVERTED-VIN-ACCUMULATION TO CONVERTED-VIN-ACCUM-OUT
           MOVE ZERO TO CONVERTED-VIN-ACCUMULATION.
           WRITE VIN-REPORT-REC FROM PROGRAM-DETAIL-LINE.
           
       250-LOAD-LETTER-TABLE.
           READ VIN-LETTER-FILE
               AT END CLOSE VIN-LETTER-FILE
               NOT AT END
                   MOVE LETTER  TO WS-LETTER (LETTER-SUB)
                   MOVE NUMERIC-VALUE TO WS-NUMERIC-VALUE (LETTER-SUB)
           END-READ.
       260-LOAD-WEIGHT-TABLE.
           READ VIN-WEIGHT-FILE
               AT END CLOSE VIN-WEIGHT-FILE
               NOT AT END
                   MOVE VIN-WEIGHT-VALUE TO WS-VIN-WEIGHT-VALUE (WEIGHT-SUB)
           END-READ.
       300-MOVE-VIN.
           PERFORM VARYING VIN-SUB FROM 1 BY 1 
               UNTIL VIN-SUB > 17
               MOVE VIN-IN (VIN-SUB) TO VIN-OUT (VIN-SUB)
           END-PERFORM.
           
       310-CHECK-PARKING.
           MOVE VIN-IN (YEAR-IDENTIFIER-SUB) TO YEAR-CHECK
           MOVE VIN-IN (CONTINENT-SUB) TO CONTINENT-CHECK
           
           IF (VIN-IN (YEAR-DIVISION-SUB) IS NUMERIC)
               IF (IS-NEWER-CAR AND IS-NORTH-AMERICA AND CHECK-DIGIT = CALCULATED-CHECK-DIGIT)
                   MOVE 'A' TO PARKING-STATUS
               ELSE
                   IF (CHECK-DIGIT = CALCULATED-CHECK-DIGIT AND IS-NORTH-AMERICA)
                       MOVE 'B' TO PARKING-STATUS
                   ELSE
                       IF (NOT IS-NORTH-AMERICA)
                           MOVE 'B' TO PARKING-STATUS
                       ELSE
                           IF (CHECK-DIGIT NOT = CALCULATED-CHECK-DIGIT)
                               MOVE 'ERROR' TO PARKING-STATUS
                           END-IF
                       END-IF
                   END-IF
               END-IF
           ELSE 
               MOVE 'A' TO PARKING-STATUS
           END-IF.
           
       320-CONVERT-TO-NUMERIC.
           PERFORM VARYING VIN-SUB FROM 1 BY 1
               UNTIL VIN-SUB > 17
               IF(VIN-IN (VIN-SUB) IS NOT NUMERIC)
                   SET LETTER-INDEX TO 1
                   SEARCH WS-LETTER-CONVERSION-PAIRS
                       WHEN VIN-IN (VIN-SUB) = WS-LETTER (LETTER-INDEX)
                           MOVE WS-NUMERIC-VALUE(LETTER-INDEX) TO CONVERTED-VIN (VIN-SUB)
                   END-SEARCH
               ELSE
                   MOVE VIN-IN (VIN-SUB) TO CONVERTED-VIN (VIN-SUB)
               END-IF
           END-PERFORM.
       400-CALCULATE-CHECK-DIGIT.
           IF (VIN-IN (CHECK-DIGIT-SUB) = 'X')
               MOVE 10 TO CHECK-DIGIT
           ELSE
               MOVE VIN-IN (CHECK-DIGIT-SUB) TO CHECK-DIGIT
           END-IF
           PERFORM VARYING WEIGHT-SUB FROM 1 BY 1
               UNTIL WEIGHT-SUB > 17
               COMPUTE CONVERTED-VIN-ACCUMULATION = CONVERTED-VIN-ACCUMULATION + 
                   (CONVERTED-VIN (WEIGHT-SUB) * WS-VIN-WEIGHT-VALUE (WEIGHT-SUB))
           END-PERFORM
           DIVIDE CONVERTED-VIN-ACCUMULATION BY CHECK-DIGIT-DIVISOR
           GIVING RESULT-OF-DIVISION
           REMAINDER CALCULATED-CHECK-DIGIT
               
       END PROGRAM PROGRAM12.