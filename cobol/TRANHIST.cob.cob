       IDENTIFICATION DIVISION.                                         00010000
       PROGRAM-ID. TRANHIST.                                            00020000
       ENVIRONMENT DIVISION.                                            00030000
       CONFIGURATION SECTION.                                           00040000
       DATA DIVISION.                                                   00050000
       WORKING-STORAGE SECTION.                                         00060000
      *----------------------------------------------------------------*00070000
      * Common defintions                                              *00080000
      *----------------------------------------------------------------*00090000
      * Run time (debug) infomation for this invocation                 00100000
        01  WS-HEADER.                                                  00110000
           03 WS-EYECATCHER            PIC X(16)                        00120000
                                        VALUE 'TRANHIST=---WS'.         00130000
           03 WS-TRANSID               PIC X(4).                        00140000
           03 WS-TERMID                PIC X(4).                        00150000
           03 WS-TASKNUM               PIC 9(7).                        00160000
           03 WS-CALEN                 PIC S9(4) COMP.                  00170000
                                                                        00180000
      * Error Message structure                                         00190000
       01  ERROR-MSG.                                                   00200000
           03 EM-DATE                  PIC X(8)  VALUE SPACES.          00210000
           03 FILLER                   PIC X     VALUE SPACES.          00220000
           03 EM-TIME                  PIC X(6)  VALUE SPACES.          00230000
           03 FILLER                   PIC X(9)  VALUE ' EXMPCMAN'.     00240000
           03 FILLER                   PIC X(11) VALUE ' REQUESTID='.   00250000
           03 EM-REQUEST-ID            PIC X(6)  VALUE SPACES.          00260000
           03 FILLER                   PIC X     VALUE SPACES.          00270000
           03 EM-DETAIL                PIC X(50) VALUE SPACES.          00280000
                                                                        00290000
      * Working variables                                               00300000
       01 WS-REQ.                                                       00310000
         03 WS-ACCT-NO             PIC 9(15).                           00320000
                                                                        00330000
       01 WS-RES.                                                       00340000
         03 WS-TRANS-ITEM OCCURS 10 TIMES.                              00350000
           05 WS-TRANS-ID          PIC X(15).                           00360000
           05 WS-TRANS-DATE        PIC X(10).                           00370000
           05 WS-TRANS-REF         PIC X(20).                           00380000
           05 WS-TRANS-AMOUNT      PIC X(11).                           00390000
           05 WS-TRANS-TYPE        PIC X(2).                            00400000
           05 WS-BALANCE           PIC X(11).                           00410000
                                                                        00420000
       01 WS-I                     PIC S9(4) COMP VALUE ZERO.           00430000
       01 WS-TRNQRY                PIC X(08) VALUE 'TRANQURY'.          00440000
                                                                        00450000
      *01 WS-PTR1                  USAGE IS POINTER.                    00460000
      *01 WS-PTR2                  USAGE IS POINTER.                    00470000
                                                                        00480000
      ******************************************************************00490000
      *    L I N K A G E   S E C T I O N                                00500000
      ******************************************************************00510000
       LINKAGE SECTION.                                                 00520000
       01 DFHCOMMAREA.                                                  00530000
           COPY TRANCOPY.                                               00540000
                                                                        00550000
      ******************************************************************00560000
      *    P R O C E D U R E S                                          00570000
      ******************************************************************00580000
       PROCEDURE DIVISION.                                              00590000
                                                                        00600000
       MAINLINE SECTION.                                                00610000
                                                                        00620000
           PERFORM 1000-INITIALIZE-VARS.                                00630000
           PERFORM 2000-GET-TRAN-HISTORY.                               00640000
           PERFORM 3000-RETURN-TO-CALLER.                               00650000
                                                                        00660000
       MAINLINE-EXIT.                                                   00670000
           EXIT.                                                        00680000
                                                                        00690000
       1000-INITIALIZE-VARS SECTION.                                    00700000
                                                                        00710000
           INITIALIZE ERROR-MSG                                         00720000
                      WS-REQ                                            00730000
                      WS-RES.                                           00740000
           MOVE EIBTRNID TO WS-TRANSID.                                 00750000
           MOVE EIBTRMID TO WS-TERMID.                                  00760000
           MOVE EIBTASKN TO WS-TASKNUM.                                 00770000
                                                                        00780000
      * If NO commarea received issue an ABEND                          00790000
           IF EIBCALEN IS EQUAL TO ZERO THEN                            00800000
      *        MOVE ' NO COMMAREA RECEIVED' TO EM-DETAIL                00810000
      *        PERFORM WRITE-ERROR-MESSAGE                              00820000
               EXEC CICS ABEND ABCODE('EXCA') NODUMP END-EXEC           00830000
           ELSE                                                         00840000
               MOVE DFHCOMMAREA(1:15) TO WS-ACCT-NO                     00850000
               DISPLAY 'Input A/C no:' WS-ACCT-NO                       00860000
           END-IF.                                                      00870000
                                                                        00880000
       1000-INITIALIZE-VARS-EXIT.                                       00890000
           EXIT.                                                        00900000
                                                                        00910000
       2000-GET-TRAN-HISTORY SECTION.                                   00920000
                                                                        00930000
      *    CALL 'TRANQURY' USING WS-REQ WS-RES.                         00940000
                                                                        00950000
      *    CALL WS-TRNQRY USING WS-REQ WS-RES.                          00960000
           CALL WS-TRNQRY USING DFHEIBLK DFHCOMMAREA WS-REQ WS-RES.     00970000
                                                                        00980000
           MOVE 0 TO WS-I.                                              00990000
                                                                        01000000
      *    Move work variable to comm area..                            01010000
           PERFORM 10 TIMES                                             01020000
              ADD +1 TO WS-I                                            01030000
              MOVE WS-TRANS-ID(WS-I)     TO TR-TRANS-ID(WS-I)           01040000
              MOVE WS-TRANS-DATE(WS-I)   TO TR-TRANS-DATE(WS-I)         01050000
              MOVE WS-TRANS-REF(WS-I)    TO TR-TRANS-REF(WS-I)          01060000
              MOVE WS-TRANS-AMOUNT(WS-I)                                01070000
                                         TO TR-TRANS-AMOUNT(WS-I)       01080000
              MOVE WS-TRANS-TYPE(WS-I)   TO TR-TRANS-TYPE(WS-I)         01090000
              MOVE WS-BALANCE(WS-I)      TO TR-BALANCE(WS-I)            01100000
      *       DISPLAY 'Copied-CA:'  TR-TRANS-ITEM(WS-I)                 01110000
           END-PERFORM.                                                 01120000
                                                                        01130000
       2000-GET-TRAN-HISTORY-EXIT.                                      01140000
           EXIT.                                                        01150000
                                                                        01160000
       3000-RETURN-TO-CALLER SECTION.                                   01170000
                                                                        01180000
           EXEC CICS RETURN END-EXEC.                                   01190000
                                                                        01200000
       3000-RETURN-TO-CALLER-EXIT.                                      01210000
           EXIT.                                                        01220000