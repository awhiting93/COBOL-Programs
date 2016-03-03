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
       PROGRAM-ID. PROGRAM02
       AUTHOR.     Whiting
       DATE-WRITTEN. JANUARY 27,2014.
      *This program is copied from the textbook page 23.
      *This program calculates the weekly wages for employees.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. SELECT EMPLOYEE-DATA
               ASSIGN TO "EMP.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
             SELECT PAYROLL-LISTING
               ASSIGN TO "WhitingEMP.LIS"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-DATA.
       01  EMPLOYEE-RECORD.
           05  EMPLOYEE-NAME-IN      PICTURE X(20).
           05  HOURS-WORKED-IN       PICTURE 9(2).
           05  HOURLY-RATE-IN        PICTURE 9V99.
           
       FD  PAYROLL-LISTING.
       01  PRINT-REC.
           05                        PICTURE X(20).
           05  NAME-OUT              PICTURE X(20).
           05                        PICTURE X(10).
           05  HOURS-OUT             PICTURE 9(2).
           05                        PICTURE X(8).
           05  RATE-OUT              PICTURE 9.99.
           05                        PICTURE X(6).
           05  WEEKLY-WAGES-OUT      PICTURE 999.99.
       
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS    PICTURE XXX VALUE 'YES'.
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT  EMPLOYEE-DATA
                OUTPUT PAYROLL-LISTING
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ EMPLOYEE-DATA
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-WAGE-ROUTINE
               END-READ
           END-PERFORM
           CLOSE EMPLOYEE-DATA
                 PAYROLL-LISTING
           STOP RUN.
       200-WAGE-ROUTINE.
           MOVE SPACES TO PRINT-REC
           MOVE EMPLOYEE-NAME-IN TO NAME-OUT
           MOVE HOURS-WORKED-IN TO HOURS-OUT
           MOVE HOURLY-RATE-IN TO RATE-OUT
           MULTIPLY HOURS-WORKED-IN BY HOURLY-RATE-IN
               GIVING WEEKLY-WAGES-OUT
           WRITE PRINT-REC.
       END PROGRAM PROGRAM02.