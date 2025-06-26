       IDENTIFICATION DIVISION.
       PROGRAM-ID. DetectDuplicates.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TransactionFile ASSIGN TO 'sample_transactions.csv'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  TransactionFile.
       01  InputRecord           PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-CONSTANTS.
           05  WS-MAX-TRANSACTIONS PIC 9(3) VALUE 100.
           
       01  WS-COUNTERS.
           05  WS-RECORD-COUNT     PIC 9(3) VALUE 0.
           05  WS-DUPLICATE-COUNT  PIC 9(3) VALUE 0.
           05  WS-LOOP-INDEX       PIC 9(3) VALUE 0.
           05  WS-INNER-INDEX      PIC 9(3) VALUE 0.
           
       01  WS-FLAGS.
           05  WS-EOF-FLAG         PIC X VALUE 'N'.
           05  WS-DUPLICATE-FOUND  PIC X VALUE 'N'.
           05  WS-FIRST-RECORD     PIC X VALUE 'Y'.
           
       01  WS-PARSED-RECORD.
           05  WS-TRANSACTION-ID   PIC X(10).
           05  WS-AMOUNT           PIC X(10).
           05  WS-DATE             PIC X(10).
           
       01  WS-TRANSACTION-TABLE.
           05  WS-TRANSACTION OCCURS 100 TIMES INDEXED BY TX-INDEX.
               10  TX-ID           PIC X(10).
               10  TX-AMOUNT       PIC X(10).
               10  TX-DATE         PIC X(10).
               10  TX-IS-DUPLICATE PIC X VALUE 'N'.
               
       01  WS-DUPLICATE-SUMMARY.
           05  WS-UNIQUE-DUPLICATES OCCURS 50 TIMES.
               10  DUP-ID          PIC X(10).
               10  DUP-COUNT       PIC 9(2).
               
       01  WS-WORK-FIELDS.
           05  WS-COMMA-POS1       PIC 9(2).
           05  WS-COMMA-POS2       PIC 9(2).
           05  WS-FIELD-START      PIC 9(2).
           05  WS-FIELD-LENGTH     PIC 9(2).

       PROCEDURE DIVISION.
       
       0000-MAIN-PROCESS.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-FILE
           PERFORM 3000-IDENTIFY-DUPLICATES
           PERFORM 4000-DISPLAY-RESULTS
           PERFORM 9000-CLEANUP
           STOP RUN.

       1000-INITIALIZE.
           DISPLAY "Payment Duplicate Detection System"
           DISPLAY "======================================"
           OPEN INPUT TransactionFile.

       2000-PROCESS-FILE.
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ TransactionFile INTO InputRecord
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       IF WS-FIRST-RECORD = 'Y'
                           MOVE 'N' TO WS-FIRST-RECORD
                       ELSE
                           PERFORM 2100-PARSE-CSV-RECORD
                           PERFORM 2200-STORE-TRANSACTION
                       END-IF
               END-READ
           END-PERFORM.

       2100-PARSE-CSV-RECORD.
           INITIALIZE WS-PARSED-RECORD
           
           MOVE 1 TO WS-FIELD-START
           PERFORM 2110-FIND-FIRST-COMMA
           COMPUTE WS-FIELD-LENGTH = WS-COMMA-POS1 - WS-FIELD-START
           MOVE InputRecord(WS-FIELD-START:WS-FIELD-LENGTH) 
               TO WS-TRANSACTION-ID
           
           COMPUTE WS-FIELD-START = WS-COMMA-POS1 + 1
           PERFORM 2120-FIND-SECOND-COMMA
           COMPUTE WS-FIELD-LENGTH = WS-COMMA-POS2 - WS-FIELD-START
           MOVE InputRecord(WS-FIELD-START:WS-FIELD-LENGTH) 
               TO WS-AMOUNT
           
           COMPUTE WS-FIELD-START = WS-COMMA-POS2 + 1
           MOVE InputRecord(WS-FIELD-START:10) TO WS-DATE.

       2110-FIND-FIRST-COMMA.
           PERFORM VARYING WS-COMMA-POS1 FROM 1 BY 1
               UNTIL WS-COMMA-POS1 > 80 
                  OR InputRecord(WS-COMMA-POS1:1) = ','
           END-PERFORM.

       2120-FIND-SECOND-COMMA.
           COMPUTE WS-COMMA-POS2 = WS-COMMA-POS1 + 1
           PERFORM VARYING WS-COMMA-POS2 FROM WS-COMMA-POS2 BY 1
               UNTIL WS-COMMA-POS2 > 80 
                  OR InputRecord(WS-COMMA-POS2:1) = ','
           END-PERFORM.

       2200-STORE-TRANSACTION.
           ADD 1 TO WS-RECORD-COUNT
           IF WS-RECORD-COUNT <= WS-MAX-TRANSACTIONS
               MOVE WS-TRANSACTION-ID TO TX-ID(WS-RECORD-COUNT)
               MOVE WS-AMOUNT TO TX-AMOUNT(WS-RECORD-COUNT)
               MOVE WS-DATE TO TX-DATE(WS-RECORD-COUNT)
           ELSE
               DISPLAY "Warning: Maximum transactions exceeded"
           END-IF.

       3000-IDENTIFY-DUPLICATES.
           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > WS-RECORD-COUNT
               
               PERFORM VARYING WS-INNER-INDEX FROM WS-LOOP-INDEX BY 1
                   UNTIL WS-INNER-INDEX > WS-RECORD-COUNT
                   
                   IF WS-LOOP-INDEX NOT = WS-INNER-INDEX
                       IF TX-ID(WS-LOOP-INDEX) = TX-ID(WS-INNER-INDEX)
                           MOVE 'Y' TO TX-IS-DUPLICATE(WS-LOOP-INDEX)
                           MOVE 'Y' TO TX-IS-DUPLICATE(WS-INNER-INDEX)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
           
           PERFORM 3100-COUNT-UNIQUE-DUPLICATES.

       3100-COUNT-UNIQUE-DUPLICATES.
           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > WS-RECORD-COUNT
               
               IF TX-IS-DUPLICATE(WS-LOOP-INDEX) = 'Y'
                   MOVE 'N' TO WS-DUPLICATE-FOUND
                   PERFORM VARYING WS-INNER-INDEX FROM 1 BY 1
                       UNTIL WS-INNER-INDEX > WS-DUPLICATE-COUNT
                          OR WS-DUPLICATE-FOUND = 'Y'
                       
                       IF DUP-ID(WS-INNER-INDEX) = TX-ID(WS-LOOP-INDEX)
                           ADD 1 TO DUP-COUNT(WS-INNER-INDEX)
                           MOVE 'Y' TO WS-DUPLICATE-FOUND
                       END-IF
                   END-PERFORM
                   
                   IF WS-DUPLICATE-FOUND = 'N'
                       ADD 1 TO WS-DUPLICATE-COUNT
                       MOVE TX-ID(WS-LOOP-INDEX) 
                           TO DUP-ID(WS-DUPLICATE-COUNT)
                       MOVE 1 TO DUP-COUNT(WS-DUPLICATE-COUNT)
                   END-IF
               END-IF
           END-PERFORM.

       4000-DISPLAY-RESULTS.
           DISPLAY " "
           DISPLAY "Processing Summary:"
           DISPLAY "Total records processed: " WS-RECORD-COUNT
           DISPLAY " "
           
           IF WS-DUPLICATE-COUNT > 0
               DISPLAY "Duplicate Transactions Found:"
               DISPLAY "============================="
               PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
                   UNTIL WS-LOOP-INDEX > WS-DUPLICATE-COUNT
                   DISPLAY "Transaction ID: " DUP-ID(WS-LOOP-INDEX)
                       " (appears " DUP-COUNT(WS-LOOP-INDEX) " times)"
               END-PERFORM
               
               DISPLAY " "
               DISPLAY "Detailed Duplicate Records:"
               DISPLAY "=========================="
               PERFORM 4100-DISPLAY-DUPLICATE-DETAILS
           ELSE
               DISPLAY "No duplicate transactions found."
           END-IF.

       4100-DISPLAY-DUPLICATE-DETAILS.
           PERFORM VARYING WS-LOOP-INDEX FROM 1 BY 1
               UNTIL WS-LOOP-INDEX > WS-RECORD-COUNT
               
               IF TX-IS-DUPLICATE(WS-LOOP-INDEX) = 'Y'
                   DISPLAY "ID: " TX-ID(WS-LOOP-INDEX)
                       " | Amount: " TX-AMOUNT(WS-LOOP-INDEX)
                       " | Date: " TX-DATE(WS-LOOP-INDEX)
               END-IF
           END-PERFORM.

       9000-CLEANUP.
           CLOSE TransactionFile
           DISPLAY " "
           DISPLAY "Processing completed successfully.".
           