       IDENTIFICATION DIVISION.
       PROGRAM-ID. YACHT.
       AUTHOR. NixieLake.
       DATE-WRITTEN. 2026-04-17
      *    This program calculates the Yacht score, given a set of five
      *    dice and a category.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      * Constants
       01 CONSTANT-VALUES.
           05 WS-CONST-ALL-SAME-DIVISOR     PIC 9(5) VALUE 11111.
           05 WS-CONST-NUM-DICE             PIC 9 VALUE 5.
           05 WS-CONST-DIE-VALUES.
               10 WS-CONST-DV-ONE           PIC 9 VALUE 1.
               10 WS-CONST-DV-TWO           PIC 9 VALUE 2.
               10 WS-CONST-DV-THREE         PIC 9 VALUE 3.
               10 WS-CONST-DV-FOUR          PIC 9 VALUE 4.
               10 WS-CONST-DV-FIVE          PIC 9 VALUE 5.
               10 WS-CONST-DV-SIX           PIC 9 VALUE 6.
           05 WS-CONST-SET-SCORES.
               10 WS-CONST-SCORE-YACHT      PIC 99 VALUE 50.
      *
      * Input/Output
       01 WS-RESULT                         PIC 99 VALUE 0.
       01 WS-CATEGORY                       PIC X(15).
       01 WS-DICE                           PIC 9(5).
      *
      * Evaluation
       01 WS-DICE-LIST REDEFINES WS-DICE.
           05 WS-DIE                        PIC 9 OCCURS 5 TIMES.
       01 WS-DICE-INDEX                     PIC 9 VALUE 0.
       01 WS-DIE-VALUE-NEEDED               PIC 9 VALUE 0.
      *
      * Calculation
       01 WS-CALCULATIONS.
           05 WS-QUOTIENT                   PIC 9 VALUE 0.
           05 WS-REMAINDER                  PIC 9(5) VALUE 0.
      *
       PROCEDURE DIVISION.
      *
       YACHT.
      * Main program control.
      *
      *    Reset score.
           MOVE ZERO                        TO WS-RESULT.
      *    Calculate score based on category.
           EVALUATE WS-CATEGORY
               WHEN "yacht" PERFORM SCORE-YACHT
               WHEN "ones" PERFORM SCORE-ONES
               WHEN "twos" PERFORM SCORE-TWOS
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
       SCORE-ONES.
      * Calculates score for Ones category.
      *
      *    Set the die value needed to 1.
           MOVE WS-CONST-DV-ONE             TO WS-DIE-VALUE-NEEDED.
      *    Iterate through each die.
           PERFORM CHECK-DIE-VALUE VARYING WS-DICE-INDEX FROM 1 BY 1
               UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE.
      *
       SCORE-TWOS.
      * Calculates score for Ones category.
      *
      *    Set the die value needed to 2.
           MOVE WS-CONST-DV-TWO             TO WS-DIE-VALUE-NEEDED.
      *    Iterate through each die.
           PERFORM CHECK-DIE-VALUE VARYING WS-DICE-INDEX FROM 1 BY 1
               UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE.
      *
       CHECK-DIE-VALUE.
      * Checks if die value at index is the value needed. If so, adds
      * the value to the score.
      *
      *    Check if die value is value needed.
           IF WS-DIE(WS-DICE-INDEX) IS EQUAL TO WS-DIE-VALUE-NEEDED THEN
      *        If so, add value to score.
               ADD WS-DIE-VALUE-NEEDED      TO WS-RESULT
           END-IF.
