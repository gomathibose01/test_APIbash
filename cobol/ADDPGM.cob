       IDENTIFICATION DIVISION.                                         00010001
       PROGRAM-ID. ADDPGM.                                              00020001
       ENVIRONMENT DIVISION.                                            00030001
       DATA DIVISION.                                                   00040001
       WORKING-STORAGE SECTION.                                         00040002
       01 WS-X    PIC 9(02).
       01 WS-Y    PIC 9(02).
       01 WS-Z    PIC 9(03).
       01 WS-TOT  PIC 9(03).
       PROCEDURE DIVISION.                                              00060001
           ACCEPT WS-X.
           ACCEPT WS-Y.
           ACCEPT WS-Z.
           ACCEPT WS-S.
           COMPUTE WS-TOT = WS-X + WS-Y.                                00060004
           DISPLAY 'SUM OF ' WS-X  ' AND 'WS-Y ' IS:' WS-TOT.            00070001
           IF WS-TOT NOT = WS-Z THEN
              MOVE +12  TO RETURN-CODE
           ELSE
              MOVE ZERO TO RETURN-CODE.
           GOBACK.                                                      00080001
