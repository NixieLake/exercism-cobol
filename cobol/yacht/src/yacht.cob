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
           05 WS-CONST-3OAK-INDEX-LIMIT     PIC 9 VALUE 3.
           05 WS-CONST-2OAK-INDEX-LIMIT     PIC 9 VALUE 4.
           05 WS-CONST-4OAK-INDEX-LIMIT     PIC 9 VALUE 2.
           05 WS-LITTLE-STRAIGHT-START      PIC 9 VALUE 1.
           05 WS-CONST-DIE-VALUES.
               10 WS-CONST-DV-ONE           PIC 9 VALUE 1.
               10 WS-CONST-DV-TWO           PIC 9 VALUE 2.
               10 WS-CONST-DV-THREE         PIC 9 VALUE 3.
               10 WS-CONST-DV-FOUR          PIC 9 VALUE 4.
               10 WS-CONST-DV-FIVE          PIC 9 VALUE 5.
               10 WS-CONST-DV-SIX           PIC 9 VALUE 6.
           05 WS-CONST-SET-SCORES.
               10 WS-CONST-SCORE-YACHT      PIC 99 VALUE 50.
               10 WS-CONST-SCORE-L-STRAIGHT PIC 99 VALUE 30.
      *
      * Input/Output
       01 WS-RESULT                         PIC 99 VALUE 0.
       01 WS-CATEGORY                       PIC X(15).
       01 WS-DICE                           PIC 9(5).
      *
      * Evaluation
       01 WS-DICE-LIST REDEFINES WS-DICE.
           05 WS-DIE                        PIC 9 OCCURS 5 TIMES.
       01 WS-DICE-INDEX-START               PIC 9 VALUE 0.
       01 WS-DICE-INDEX                     PIC 9 VALUE 0.
       01 WS-DIE-VALUE-NEEDED               PIC 9 VALUE 0.
       01 WS-THREE-OF-A-KIND-VALUE          PIC 9 VALUE 0.
       01 WS-TWO-OF-ANOTHER-VALUE           PIC 9 VALUE 0.
       01 WS-FOUR-OF-A-KIND-VALUE           PIC 9 VALUE 0.
       01 WS-NUMBER-FOUND                   PIC 9 VALUE 0.
       01 WS-COUNT                          PIC 9 VALUE 0.
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
      *    Reset values.
           MOVE ZERO                        TO WS-RESULT,
                                               WS-DIE-VALUE-NEEDED,
                                               WS-DICE-INDEX-START,
                                               WS-THREE-OF-A-KIND-VALUE,
                                               WS-TWO-OF-ANOTHER-VALUE,
                                               WS-FOUR-OF-A-KIND-VALUE,
                                               WS-NUMBER-FOUND.
      *
      *    Calculate score based on category.
           EVALUATE WS-CATEGORY
               WHEN "yacht" PERFORM SCORE-YACHT
               WHEN "ones" PERFORM SCORE-ONES
               WHEN "twos" PERFORM SCORE-TWOS
               WHEN "threes" PERFORM SCORE-THREES
               WHEN "fours" PERFORM SCORE-FOURS
               WHEN "fives" PERFORM SCORE-FIVES
               WHEN "sixes" PERFORM SCORE-SIXES
               WHEN "full house" PERFORM SCORE-FULL-HOUSE
               WHEN "four of a kind" PERFORM SCORE-FOUR-OF-A-KIND
               WHEN "little straight" PERFORM SCORE-LITTLE-STRAIGHT
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
      * Calculates score for Twos category.
      *
      *    Set the die value needed to 2.
           MOVE WS-CONST-DV-TWO             TO WS-DIE-VALUE-NEEDED.
      *    Iterate through each die.
           PERFORM CHECK-DIE-VALUE VARYING WS-DICE-INDEX FROM 1 BY 1
             UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE.
      *
       SCORE-THREES.
      * Calculates score for Threes category.
      *
      *    Set the die value needed to 3.
           MOVE WS-CONST-DV-THREE           TO WS-DIE-VALUE-NEEDED.
      *    Iterate through each die.
           PERFORM CHECK-DIE-VALUE VARYING WS-DICE-INDEX FROM 1 BY 1
             UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE.
      *
       SCORE-FOURS.
      * Calculates score for Fours category.
      *
      *    Set the die value needed to 4.
           MOVE WS-CONST-DV-FOUR            TO WS-DIE-VALUE-NEEDED.
      *    Iterate through each die.
           PERFORM CHECK-DIE-VALUE VARYING WS-DICE-INDEX FROM 1 BY 1
             UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE.
      *
       SCORE-FIVES.
      * Calculates score for Fives category.
      *
      *    Set the die value needed to 5.
           MOVE WS-CONST-DV-FIVE            TO WS-DIE-VALUE-NEEDED.
      *    Iterate through each die.
           PERFORM CHECK-DIE-VALUE VARYING WS-DICE-INDEX FROM 1 BY 1
             UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE.
      *
       SCORE-SIXES.
      * Calculates score for Sixes category.
      *
      *    Set the die value needed to 6.
           MOVE WS-CONST-DV-SIX             TO WS-DIE-VALUE-NEEDED.
      *    Iterate through each die.
           PERFORM CHECK-DIE-VALUE VARYING WS-DICE-INDEX FROM 1 BY 1
             UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE.
      *
       SCORE-FULL-HOUSE.
      * Checks for a full house (3 of one kind and 2 of another). If so,
      * totals dice values for score.
      *
      *    Look for 3-of-a-kind.
           PERFORM FIND-3-OF-A-KIND.
      *    If no 3-of-a-kind, it's not a full house, so exit.
           IF WS-THREE-OF-A-KIND-VALUE IS ZERO THEN
               EXIT PARAGRAPH
           END-IF.
      *    Look for 2-of-a-kind different from 3-of-a-kind.
           PERFORM FIND-2-OF-ANOTHER.
      *    If no 2-of-another, it's not a full house, so exit.
           IF WS-TWO-OF-ANOTHER-VALUE IS ZERO THEN
               EXIT PARAGRAPH
           END-IF.
      *    It's a full house, so add values to score.
           PERFORM ADD-3-OF-A-KIND-SCORE.
           PERFORM ADD-2-OF-ANOTHER-SCORE.
      *
       SCORE-FOUR-OF-A-KIND.
      * Checks for four-of-a-kind. If so, totals the score of the four
      * dice.
      *
      *    *    Iterate over first 2 dice as starting die.
           PERFORM VARYING WS-DICE-INDEX-START FROM 1 BY 1
             UNTIL WS-DICE-INDEX-START > WS-CONST-4OAK-INDEX-LIMIT
      *        Reset number found and set value needed to current start.
               MOVE 0                       TO WS-NUMBER-FOUND
               MOVE WS-DIE(WS-DICE-INDEX-START) 
                   TO WS-DIE-VALUE-NEEDED
      *        Iterate over remaining dice looking for match.
               PERFORM VARYING WS-DICE-INDEX 
                 FROM WS-DICE-INDEX-START BY 1
                 UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE
      *            If match found, add to number found.
                   IF WS-DIE(WS-DICE-INDEX) 
                     IS EQUAL TO WS-DIE-VALUE-NEEDED THEN
                       ADD 1                TO WS-NUMBER-FOUND
                   END-IF
               END-PERFORM
      *        If number found > 3 then set 4-of-a-kind value.
               IF WS-NUMBER-FOUND IS GREATER THAN 3 THEN
                   MOVE WS-DIE-VALUE-NEEDED TO WS-FOUR-OF-A-KIND-VALUE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      *    Add score of 4 times the 4-of-a-kind value.
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT > 4
               ADD WS-FOUR-OF-A-KIND-VALUE TO WS-RESULT
           END-PERFORM.
      *
       SCORE-LITTLE-STRAIGHT.
      * Checks for a little straight. If so, adds little straight score.
      *
      *    Iterate through each target number (1-5).
           PERFORM VARYING WS-DIE-VALUE-NEEDED 
             FROM WS-LITTLE-STRAIGHT-START BY 1
             UNTIL WS-DIE-VALUE-NEEDED > WS-CONST-NUM-DICE
      *        Reset number found to 0.
               MOVE 0                       TO WS-NUMBER-FOUND
      *        Iterate through dice, looking for target number.
               PERFORM VARYING WS-DICE-INDEX FROM 1 BY 1 
                 UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE
      *            If number found, exit loop.
                   IF WS-DIE(WS-DICE-INDEX)
                     IS EQUAL TO WS-DIE-VALUE-NEEDED THEN
                       MOVE 1               TO WS-NUMBER-FOUND
                       EXIT PERFORM
                   END-IF
               END-PERFORM
      *        If number not found, exit paragraph with no score.
               IF WS-NUMBER-FOUND IS ZERO THEN
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.
      *    If we get here, we have a little straight. Add the score.
           MOVE WS-CONST-SCORE-L-STRAIGHT   TO WS-RESULT.
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
      *
       FIND-3-OF-A-KIND.
      * Looks for 3-of-a-kind and sets value if found.
      *
      *    Iterate over first 3 dice as starting die.
           PERFORM VARYING WS-DICE-INDEX-START FROM 1 BY 1
             UNTIL WS-DICE-INDEX-START > WS-CONST-3OAK-INDEX-LIMIT
      *        Reset number found and set value needed to current start.
               MOVE 0                       TO WS-NUMBER-FOUND
               MOVE WS-DIE(WS-DICE-INDEX-START) 
                   TO WS-DIE-VALUE-NEEDED
      *        Iterate over remaining dice looking for match.
               PERFORM VARYING WS-DICE-INDEX 
                 FROM WS-DICE-INDEX-START BY 1
                 UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE
      *            If match found, add to number found.
                   IF WS-DIE(WS-DICE-INDEX) 
                     IS EQUAL TO WS-DIE-VALUE-NEEDED THEN
                       ADD 1                TO WS-NUMBER-FOUND
                   END-IF
               END-PERFORM
      *        If number found is 3 then set 3-of-a-kind value.
               IF WS-NUMBER-FOUND IS EQUAL TO 3 THEN
                   MOVE WS-DIE-VALUE-NEEDED TO WS-THREE-OF-A-KIND-VALUE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      *
       FIND-2-OF-ANOTHER.
      * Looks for 2-of-a-kind different from 3-of-a-kind value and sets
      * value if found.
      *
      *    Iterate over first 4 dice as starting die.
           PERFORM VARYING WS-DICE-INDEX-START FROM 1 BY 1
             UNTIL WS-DICE-INDEX-START > WS-CONST-2OAK-INDEX-LIMIT
      *        Reset number found and set value needed to current start.
               MOVE 0                       TO WS-NUMBER-FOUND
               MOVE WS-DIE(WS-DICE-INDEX-START) 
                   TO WS-DIE-VALUE-NEEDED
      *        If value is same as 3-of-a-kind then skip.
               IF WS-DIE-VALUE-NEEDED 
                 IS EQUAL TO WS-THREE-OF-A-KIND-VALUE THEN
                   EXIT PERFORM
               END-IF
      *        Iterate over remaining dice looking for match.
               PERFORM VARYING WS-DICE-INDEX 
                 FROM WS-DICE-INDEX-START BY 1
                 UNTIL WS-DICE-INDEX > WS-CONST-NUM-DICE
      *            If match found, add to number found.
                   IF WS-DIE(WS-DICE-INDEX) 
                     IS EQUAL TO WS-DIE-VALUE-NEEDED THEN
                       ADD 1                TO WS-NUMBER-FOUND
                   END-IF
               END-PERFORM
      *        If number found is 2 then set 2-of-another value.
               IF WS-NUMBER-FOUND IS EQUAL TO 2 THEN
                   MOVE WS-DIE-VALUE-NEEDED TO WS-TWO-OF-ANOTHER-VALUE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
      *
       ADD-3-OF-A-KIND-SCORE.
      * AddS 3-of-a-kind value to score 3 times.
      *
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT > 3
               ADD WS-THREE-OF-A-KIND-VALUE TO WS-RESULT
           END-PERFORM.
      *
       ADD-2-OF-ANOTHER-SCORE.
      * Adds 2-of-another value to score 2 times.
      *
           PERFORM VARYING WS-COUNT FROM 1 BY 1 UNTIL WS-COUNT > 2
               ADD WS-TWO-OF-ANOTHER-VALUE  TO WS-RESULT
           END-PERFORM.
