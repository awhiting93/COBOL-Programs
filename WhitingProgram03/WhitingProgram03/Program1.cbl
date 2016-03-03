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
       PROGRAM-ID. PROGRAM03
       AUTHOR.     Whiting
       DATE-WRITTEN. FEBRUARY 2, 2015.
      *This program is copied from the textbook page 61.
      *This program calculates the grade average for students.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL. 
             SELECT STUDENT-FILE
               ASSIGN TO "..\..\..\..\..\GRADES.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.
             SELECT TRANSCRIPT-FILE
               ASSIGN TO "..\..\..\..\..\WhitingTRANS.TXT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  STUDENT-FILE.
       01  STUDENT-REC.
           05  STUDENT-NAME          PIC X(20).
           05  GRADE1                PIC 999.
           05  GRADE2                PIC 999.
           05  GRADE3                PIC 999.
           
       FD  TRANSCRIPT-FILE.
       01  TRANSCRIPT-REC.
           05  NAME-OUT              PIC X(20).
           05  AVERAGE               PIC 999.
       
       WORKING-STORAGE SECTION.
       01  ARE-THERE-MORE-RECORDS    PIC X(3) VALUE 'YES'.
       
       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           OPEN INPUT  STUDENT-FILE
                OUTPUT TRANSCRIPT-FILE
           PERFORM UNTIL ARE-THERE-MORE-RECORDS = 'NO '
               READ STUDENT-FILE
                   AT END
                       MOVE 'NO ' TO ARE-THERE-MORE-RECORDS
                   NOT AT END
                       PERFORM 200-CALC-RTN
               END-READ
           END-PERFORM
           CLOSE STUDENT-FILE
                 TRANSCRIPT-FILE
           STOP RUN.
           
       200-CALC-RTN.
           MOVE STUDENT-NAME TO NAME-OUT
           ADD GRADE1, GRADE2, GRADE3
               GIVING AVERAGE
           DIVIDE 3 INTO AVERAGE
           WRITE TRANSCRIPT-REC.
       END PROGRAM PROGRAM03.