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
       PROGRAM-ID. PROGRAM1.
       AUTHOR. ANDREW WHITING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  HOURS    PIC 99.
       01  RATE     PIC 99V99.
       01  WAGES    PIC 999.99.
       01  MORE-DATA PIC XXX VALUE "YES".
       PROCEDURE DIVISION.
       100-MAIN.
           PERFORM UNTIL MORE-DATA = "NO "
               DISPLAY "ENTER HOURS AS A TWO DIGIT NUMBER"
               ACCEPT HOURS
               DISPLAY "ENTER RATE IN NN.NN FORMAT (2 DECIMAL DIGITS)"
               ACCEPT RATE
               MULTIPLY RATE BY HOURS GIVING WAGES
               DISPLAY "WAGES ARE ", WAGES
               DISPLAY "IS THERE MORE DATA (YES/NO)?"
               ACCEPT MORE-DATA
           END-PERFORM
           STOP RUN.