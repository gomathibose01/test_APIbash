       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANQURY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Working variables
       01 WS-FLAG                   PIC X(01).
          88 WS-BTCH                VALUE "B".
          88 WS-CICS                VALUE "C".

       01 WS-REQ.
          03 WS-ACCT-NO             PIC 9(15).

       01 WS-RES.
          03 WS-TRANS-ITEM OCCURS 10 TIMES.
             05 WS-TRANS-ID          PIC X(15).
             05 WS-TRANS-DATE        PIC X(10).
             05 WS-TRANS-REF         PIC X(20).
      *      05 WS-TRANS-AMOUNT      PIC X(11).
             05 WS-TRANS-AMOUNT      PIC Z(08).99.
             05 WS-TRANS-TYPE        PIC X(2).
      *      05 WS-BALANCE           PIC X(11).
             05 WS-BALANCE           PIC Z(08).99.


       01 GET-TRANS-CURSOR            PIC X(128).
       01 WS-J                        PIC S9(4) COMP VALUE ZERO.

       01 WS-HOST-IP.
           03 WS-HOST-ACCT-NO        PIC S9(9) USAGE COMP.

       01 WS-HOST-TABLE.
           03 WS-HOST-TRANS-ID       PIC S9(9) USAGE COMP
                                     OCCURS 10 TIMES.
           03 WS-HOST-TRANS-DATE     PIC X(10)
                                     OCCURS 10 TIMES.
           03 WS-HOST-TRANS-REF      PIC X(20)
                                     OCCURS 10 TIMES.
           03 WS-HOST-TRANS-AMOUNT   PIC S9(8)V9(2) USAGE COMP-3
                                     OCCURS 10 TIMES.
           03 WS-HOST-TRANS-TYPE     PIC X(02)
                                     OCCURS 10 TIMES.
           03 WS-HOST-BALANCE        PIC S9(8)V9(2) USAGE COMP-3
                                     OCCURS 10 TIMES.

           EXEC SQL
             INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
             INCLUDE TRANSDCL
           END-EXEC.

      ******************************************************************
      *    L I N K A G E   S E C T I O N
      ******************************************************************
       LINKAGE SECTION.

      *01 LK-PTR1           USAGE IS POINTER.
      *01 LK-PTR2           USAGE IS POINTER.

      *01 DFHEIBLK          PIC X(01).
       01 DFHCOMMAREA        PIC X(01).

       COPY TRANCPLK.

      ******************************************************************
      *    P R O C E D U R E S
      ******************************************************************
      *PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA LK-REQ LK-RES.
       PROCEDURE DIVISION USING DFHCOMMAREA LK-REQ LK-RES.

       MAINLINE SECTION.

           PERFORM 1000-INITIALIZE-VARS.
           PERFORM 2000-GET-TRAN-HISTORY.
           PERFORM 3000-RETURN-TO-CALLER.

       MAINLINE-EXIT.
           EXIT.

       1000-INITIALIZE-VARS SECTION.

           INITIALIZE WS-HOST-ACCT-NO.

           IF DFHCOMMAREA = 0 THEN
              DISPLAY 'LK-ACCT-NO: ' LK-ACCT-NO
              SET WS-CICS TO TRUE
           ELSE
              SET WS-BTCH TO TRUE
           END-IF.

           IF WS-CICS THEN
              MOVE LK-ACCT-NO TO WS-ACCT-NO
           ELSE
              ACCEPT WS-ACCT-NO
              DISPLAY 'WS-ACCOUNT-NO: ' WS-ACCT-NO
           END-IF.

           MOVE WS-ACCT-NO TO WS-HOST-ACCT-NO.
           DISPLAY 'WS-HOST-ACCOUNT-NO: ' WS-HOST-ACCT-NO.

       1000-INITIALIZE-VARS-EXIT.
           EXIT.

       2000-GET-TRAN-HISTORY SECTION.

           PERFORM 2100-DECLARE-CURSOR.

           PERFORM 2200-OPEN-CURSOR.

           PERFORM 2300-FETCH-CURSOR.

           PERFORM 2400-CLOSE-CURSOR.

       2000-GET-TRAN-HISTORY-EXIT.
           EXIT.

       2100-DECLARE-CURSOR SECTION.

           EXEC SQL
             DECLARE GET-TRANS-CURSOR CURSOR WITH
             ROWSET POSITIONING FOR
             SELECT
                TRANS_ID
               ,TRANS_DATE
               ,TRANS_REF
               ,TRANS_AMOUNT
               ,TRANS_TYPE
               ,BALANCE
             FROM TRANSACTIONS
             WHERE ACCOUNT_NO = :WS-HOST-ACCT-NO
           END-EXEC.

           DISPLAY 'Cursor Declared:' SQLCODE.

       2100-DECLARE-CURSOR-EXIT.
           EXIT.

       2200-OPEN-CURSOR SECTION.

           EXEC SQL
             OPEN GET-TRANS-CURSOR
           END-EXEC.

      *    DISPLAY 'Cursor Opened:' SQLCODE.

       2200-OPEN-CURSOR-EXIT.
           EXIT.

       2300-FETCH-CURSOR SECTION.

           EXEC SQL
             FETCH NEXT ROWSET FROM GET-TRANS-CURSOR FOR 10 ROWS
             INTO :WS-HOST-TRANS-ID
                 ,:WS-HOST-TRANS-DATE
                 ,:WS-HOST-TRANS-REF
                 ,:WS-HOST-TRANS-AMOUNT
                 ,:WS-HOST-TRANS-TYPE
                 ,:WS-HOST-BALANCE
           END-EXEC.

           DISPLAY 'No.of rows fetched' SQLERRD(3)

      *    Move host values to work area..
           PERFORM SQLERRD(3) TIMES
              ADD +1 TO WS-J
              MOVE WS-HOST-TRANS-ID(WS-J)     TO WS-TRANS-ID(WS-J)
              MOVE WS-HOST-TRANS-DATE(WS-J)   TO WS-TRANS-DATE(WS-J)
              MOVE WS-HOST-TRANS-REF(WS-J)    TO WS-TRANS-REF(WS-J)
              MOVE WS-HOST-TRANS-AMOUNT(WS-J)
                                              TO WS-TRANS-AMOUNT(WS-J)
              MOVE WS-HOST-TRANS-TYPE(WS-J)   TO WS-TRANS-TYPE(WS-J)
      * The below line should be uncommented when moving to prod.....
              MOVE WS-HOST-BALANCE(WS-J)      TO WS-BALANCE(WS-J)
      * The below line should be commented out when moving to prod....
      *       MOVE WS-BALANCE(WS-J)           TO WS-BALANCE(WS-J)
              DISPLAY 'Copied-WS:'  WS-TRANS-ITEM(WS-J)
           END-PERFORM.


           IF WS-CICS THEN
              MOVE 0 TO WS-J

      *    Move work variable to link area..
              PERFORM SQLERRD(3) TIMES
                 ADD +1 TO WS-J
                 MOVE WS-TRANS-ID(WS-J)     TO LK-TRANS-ID(WS-J)
                 MOVE WS-TRANS-DATE(WS-J)   TO LK-TRANS-DATE(WS-J)
                 MOVE WS-TRANS-REF(WS-J)    TO LK-TRANS-REF(WS-J)
                 MOVE WS-TRANS-AMOUNT(WS-J)
                                         TO LK-TRANS-AMOUNT(WS-J)
                 MOVE WS-TRANS-TYPE(WS-J)   TO LK-TRANS-TYPE(WS-J)
                 MOVE WS-BALANCE(WS-J)      TO LK-BALANCE(WS-J)
      *          DISPLAY 'Copied-LK:'  LK-TRANS-ITEM(WS-J)
               END-PERFORM

           END-IF.

       2300-FETCH-CURSOR-EXIT.
           EXIT.

       2400-CLOSE-CURSOR SECTION.

           EXEC SQL
             CLOSE GET-TRANS-CURSOR
           END-EXEC.

      *    DISPLAY 'Cursor Closed:' SQLCODE.

       2400-CLOSE-CURSOR-EXIT.
           EXIT.

       3000-RETURN-TO-CALLER SECTION.

      *     IF WS-CICS THEN
      *        GOBACK
      *     ELSE
      *        STOP RUN
      *     END-IF.
            GOBACK.
       3000-RETURN-TO-CALLER-EXIT.
           EXIT.