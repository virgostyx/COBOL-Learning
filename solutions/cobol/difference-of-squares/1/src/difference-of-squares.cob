       IDENTIFICATION DIVISION.
       PROGRAM-ID. difference-of-squares.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DIFFERENCE-OF-SQUARES PIC 9(8).
       01 WS-SUM-OF-SQUARES PIC 9(8).
       01 WS-SQUARE-OF-SUM PIC 9(8).
       01 WS-NUMBER PIC 9(8).

       PROCEDURE DIVISION.
       
       SQUARE-OF-SUM.
         COMPUTE WS-SQUARE-OF-SUM = 
            (WS-NUMBER * (WS-NUMBER + 1) / 2) ** 2.
        
       SUM-OF-SQUARES.
         COMPUTE WS-SUM-OF-SQUARES = 
            WS-NUMBER * (WS-NUMBER + 1) * (2 * WS-NUMBER + 1) / 6.
       
       DIFFERENCE-OF-SQUARES.
         COMPUTE WS-DIFFERENCE-OF-SQUARES =
            (WS-NUMBER * (WS-NUMBER + 1) * (WS-NUMBER - 1) 
               * ((3 * WS-NUMBER) + 2)) / 12.

