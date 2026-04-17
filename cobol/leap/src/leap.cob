       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       AUTHOR. NIXIELAKE.
       DATE-WRITTEN. 2026-04-16.
      * Description:
      *    This program determines if a given year is a leap year.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * Required variables
       01 WS-YEAR                           PIC 9(4) VALUE ZERO.
       01 WS-RESULT                         PIC 9(1) VALUE ZERO.
           88 88-IS-LEAP VALUE 1.
           88 88-NOT-LEAP VALUE 0.
      *
      * Calculation variables
       01 WS-DIVISION-RESULT                PIC 9(4) VALUE ZERO.
       01 WS-REMAINDER-4                    PIC 9(4) VALUE ZERO.
       01 WS-REMAINDER-100                  PIC 9(4) VALUE ZERO.
       01 WS-REMAINDER-400                  PIC 9(4) VALUE ZERO.
       PROCEDURE DIVISION.
       LEAP.
      * Enter solution here
      *
      *    Is year devisible by 4?
           DIVIDE WS-YEAR BY 4 
               GIVING WS-DIVISION-RESULT 
               REMAINDER WS-REMAINDER-4.
           IF WS-REMAINDER-4 IS ZERO THEN
      *        Year is divisible by 4. Check if it's divisible by 100.
               DIVIDE WS-YEAR BY 100
                   GIVING WS-DIVISION-RESULT
                   REMAINDER WS-REMAINDER-100
               IF WS-REMAINDER-100 IS ZERO THEN
      *            Year is divisible by 100. Check if it's divisible by
      *            400.
                   DIVIDE WS-YEAR BY 400
                       GIVING WS-DIVISION-RESULT
                       REMAINDER WS-REMAINDER-400
                   IF WS-REMAINDER-400 IS ZERO THEN
      *                Year is divisible by 400. It's a leap year.
                       SET 88-IS-LEAP TO TRUE
                   ELSE
      *                Year is divisible by 100 but not by 400. It's not
      *                a leap year.
                       SET 88-NOT-LEAP TO TRUE
                   END-IF
               ELSE
      *            Year is divisible by 4 but not by 100. It's a leap
      *            year.
                   SET 88-IS-LEAP TO TRUE
               END-IF
           ELSE
      *        Year is not divisible by 4. It's not a leap year.
               SET 88-NOT-LEAP TO TRUE
           END-IF.
           CONTINUE.
       LEAP-EXIT.
           EXIT.
