       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       AUTHOR. NixieLake.
       DATE-WRITTEN. 2026-04-17
      *    This program calculates the Yacht score, given a set of five
      *    dice and a category.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * Constant values.
       01 CONSTANT-VALUES.
           05 WS-CONST-ALL-SAME-DIVISOR     PIC 9(5) VALUE 11111.
           05 WS-CONST-SET-SCORES.
               10 WS-CONST-SCORE-YACHT      PIC 99 VALUE 50.
      *
      * Input/Output stores.
       01 WS-RESULT                         PIC 99 VALUE 0.
       01 WS-CATEGORY                       PIC X(15).
       01 WS-DICE                           PIC 9(5).
      *
      * Evaluation stores.
       01 WS-DICE-LIST REDEFINES WS-DICE.
           05 WS-DIE                        PIC 9 OCCURS 5 TIMES.
      *
      * Calculation stores.
       01 WS-CALCULATIONS.
           05 WS-QUOTIENT                   PIC 9 VALUE 0.
           05 WS-REMAINDER                  PIC 9(5) VALUE 0.
      *
       PROCEDURE DIVISION.
      *
       YACHT.
      * Main program control.
           EVALUATE WS-CATEGORY
               WHEN "yacht" PERFORM SCORE-YACHT
           END-EVALUATE.
           EXIT.
      *
       SCORE-YACHT.
      * Calculates score for Yacht category.
      *
      *    Determine if all dice are the same.
           DIVIDE WS-DICE BY WS-CONST-ALL-SAME-DIVISOR
               GIVING WS-QUOTIENT REMAINDER WS-REMAINDER.
      *    If all dice are the same, then score is 50.
           IF WS-REMAINDER IS ZERO THEN
               MOVE WS-CONST-SCORE-YACHT    TO WS-RESULT
      *    If all dice are not the smae, then score is 0.
           ELSE
               MOVE ZERO                    TO WS-RESULT
           END-IF.
      *
