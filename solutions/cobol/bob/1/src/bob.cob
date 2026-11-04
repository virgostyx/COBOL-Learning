       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOB.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-HEYBOB                PIC X(60).
       01 WS-RESULT                PIC X(40).

       01 WS-SURE CONSTANT AS "Sure.".
       01 WS-CHILL CONSTANT AS "Whoa, chill out!".
       01 WS-CALM  CONSTANT AS "Calm down, I know what I'm doing!".
       01 WS-FINE  CONSTANT AS "Fine. Be that way!".
       01 WS-WHATEVER CONSTANT AS "Whatever.".

       01 WS-LONGUEUR              PIC 99.

       01 WS-DERNIER               PIC X.
          88 QUESTION              VALUE "?".

       01 WS-CRI                   PIC X VALUE "N".
          88 CRIE                  VALUE "O".
          88 PAS-CRIE              VALUE "N".

       PROCEDURE DIVISION.
       BOB.
           PERFORM SELECT-BOB-ANSWER.

       SELECT-BOB-ANSWER.
           IF WS-HEYBOB = SPACES
              MOVE WS-FINE TO WS-RESULT
              EXIT PARAGRAPH
           END-IF

           COMPUTE WS-LONGUEUR =
               FUNCTION LENGTH(FUNCTION TRIM(WS-HEYBOB TRAILING))
           MOVE WS-HEYBOB(WS-LONGUEUR:1) TO WS-DERNIER

           IF WS-HEYBOB = FUNCTION UPPER-CASE(WS-HEYBOB)
              AND WS-HEYBOB NOT = FUNCTION LOWER-CASE(WS-HEYBOB)
              SET CRIE TO TRUE
           ELSE
              SET PAS-CRIE TO TRUE
           END-IF

           EVALUATE TRUE ALSO TRUE
              WHEN CRIE     ALSO QUESTION
                 MOVE WS-CALM TO WS-RESULT
              WHEN CRIE     ALSO ANY
                 MOVE WS-CHILL TO WS-RESULT
              WHEN PAS-CRIE ALSO QUESTION
                 MOVE WS-SURE TO WS-RESULT
              WHEN OTHER
                 MOVE WS-WHATEVER TO WS-RESULT
           END-EVALUATE.