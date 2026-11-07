       IDENTIFICATION DIVISION.
       PROGRAM-ID. hamming.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DNA-1 PIC X(32).
       01 WS-LONG-DNA-1 PIC 99.
       01 WS-DNA-2 PIC X(32).
       01 WS-LONG-DNA-2 PIC 99.
       01 WS-HAMMING PIC 9(2).
       01 WS-ERROR PIC X(31).

       01 WS-I PIC 99.

       PROCEDURE DIVISION.
       HAMMING.
         PERFORM CALCULER-DISTANCE.

       DISPLAY "Hamming Distance is: " WS-HAMMING.
       DISPLAY WS-ERROR.

       CALCULER-DISTANCE.
         MOVE ZERO TO WS-HAMMING
         MOVE SPACES TO WS-ERROR

         IF WS-DNA-1 = SPACES
            MOVE ZERO TO WS-LONG-DNA-1
         ELSE
            MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-1 TRAILING)) 
               TO WS-LONG-DNA-1
         END-IF

         IF WS-DNA-2 = SPACES
            MOVE ZERO TO WS-LONG-DNA-2
         ELSE
            MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DNA-2 TRAILING)) 
               TO WS-LONG-DNA-2
         END-IF

         IF WS-LONG-DNA-1 NOT = WS-LONG-DNA-2
            MOVE "Strands must be of equal length" TO WS-ERROR
         ELSE
            PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-LONG-DNA-1
               IF WS-DNA-1(WS-I:1) NOT = WS-DNA-2(WS-I:1)
                  ADD 1 TO WS-HAMMING
               END-IF
            END-PERFORM
         END-IF.

         
