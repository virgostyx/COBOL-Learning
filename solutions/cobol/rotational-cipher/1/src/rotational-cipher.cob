       IDENTIFICATION DIVISION.
       PROGRAM-ID. rotational-cipher.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-KEY PIC 9(2).
       01 WS-TEXT PIC X(128).
       01 WS-CIPHER PIC X(128).

       01 WS-MINUS        PIC X(26) VALUE "abcdefghijklmnopqrstuvwxyz".
       01 WS-MAJUS        PIC X(26) VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
       01 WS-MINUS-DEC    PIC X(26).
       01 WS-MAJUS-DEC    PIC X(26).
       01 WS-K            PIC 99.
      
       PROCEDURE DIVISION.
       ROTATIONAL-CIPHER.
         MOVE FUNCTION UPPER-CASE(WS-TEXT) TO WS-TEXT
         PERFORM CHIFFRER-TEXTE.

       CHIFFRER-TEXTE.
          MOVE WS-TEXT TO WS-CIPHER
          COMPUTE WS-K = FUNCTION MOD(WS-KEY, 26)
          IF WS-K = 0
              MOVE WS-MINUS TO WS-MINUS-DEC
              MOVE WS-MAJUS TO WS-MAJUS-DEC
          ELSE
              MOVE WS-MINUS(WS-K + 1:26 - WS-K) 
                   TO WS-MINUS-DEC(1:26 - WS-K)
              MOVE WS-MINUS(1:WS-K)             
                   TO WS-MINUS-DEC(27 - WS-K:WS-K)
              MOVE WS-MAJUS(WS-K + 1:26 - WS-K) 
                   TO WS-MAJUS-DEC(1:26 - WS-K)
              MOVE WS-MAJUS(1:WS-K)             
                   TO WS-MAJUS-DEC(27 - WS-K:WS-K)
          END-IF
      
          INSPECT WS-CIPHER CONVERTING WS-MINUS TO WS-MINUS-DEC
          INSPECT WS-CIPHER CONVERTING WS-MAJUS TO WS-MAJUS-DEC.