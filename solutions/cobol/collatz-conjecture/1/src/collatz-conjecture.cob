       IDENTIFICATION DIVISION.
       PROGRAM-ID. collatz-conjecture.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMBER PIC S9(8).
       01 WS-STEPS PIC 9(4).
       01 WS-ERROR PIC X(35).

       01 WS-RESTE   PIC 9.
         88 NOMBRE-PAIR      VALUE 0.
         88 NOMBRE-IMPAIR    VALUE 1.
       01 WS-QUOTIENT PIC 9(8) VALUE  ZERO.

       PROCEDURE DIVISION.
       COLLATZ-CONJECTURE.
         MOVE ZERO   TO WS-STEPS
         MOVE SPACES TO WS-ERROR
      
         IF WS-NUMBER < 1 
            MOVE "Only positive integers are allowed" TO WS-ERROR
         ELSE
            PERFORM UNTIL WS-NUMBER = 1
               DIVIDE WS-NUMBER BY 2 
                      GIVING WS-QUOTIENT 
                      REMAINDER WS-RESTE
               IF NOMBRE-PAIR
                  MOVE WS-QUOTIENT TO WS-NUMBER
               ELSE
                  MULTIPLY 3 BY WS-NUMBER
                  ADD 1 TO WS-NUMBER
               END-IF

               ADD 1 TO WS-STEPS
            END-PERFORM
         END-IF.

         DISPLAY WS-STEPS.
         DISPLAY WS-ERROR.

       
