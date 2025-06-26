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
       01  TransactionRecord.
           05  TransactionID     PIC X(10).
           05  TransactionAmount PIC 9(7)V99.
           05  TransactionDate   PIC X(10).

       WORKING-STORAGE SECTION.
       01  DuplicateTransactions.
           05  DuplicateID       PIC X(10) OCCURS 100 TIMES.
           05  DuplicateCount    PIC 9(3) VALUE 0.
       01  CurrentID            PIC X(10).
       01  RecordCounter        PIC 9(3) VALUE 0.
       01  EndOfFile           PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT TransactionFile
           PERFORM UNTIL EndOfFile = 'Y'
               READ TransactionFile INTO TransactionRecord
                   AT END
                       MOVE 'Y' TO EndOfFile
                   NOT AT END
                       PERFORM CHECK-DUPLICATES
               END-READ
           END-PERFORM
           CLOSE TransactionFile
           DISPLAY "Duplicate Transactions:"
           PERFORM DISPLAY-DUPLICATES
           STOP RUN.

       CHECK-DUPLICATES.
           MOVE TransactionID TO CurrentID
           IF DuplicateCount = 0
               MOVE CurrentID TO DuplicateID(DuplicateCount)
               ADD 1 TO DuplicateCount
           ELSE
               PERFORM VARYING RecordCounter FROM 1 BY 1
                   UNTIL RecordCounter > DuplicateCount
                   IF CurrentID = DuplicateID(RecordCounter)
                       DISPLAY "Duplicate Found: " CurrentID
                       EXIT PERFORM
                   END-IF
               END-PERFORM
               IF RecordCounter > DuplicateCount
                   MOVE CurrentID TO DuplicateID(DuplicateCount)
                   ADD 1 TO DuplicateCount
               END-IF
           END-IF.

       DISPLAY-DUPLICATES.
           PERFORM VARYING RecordCounter FROM 1 BY 1
               UNTIL RecordCounter > DuplicateCount
               DISPLAY DuplicateID(RecordCounter)
           END-PERFORM.
           