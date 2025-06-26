# COBOL Payment Duplicates Project

This project contains a COBOL script that reads a sample CSV file containing payment transactions, detects duplicate transactions, and prints the duplicate transactions to the screen.

## Project Structure

```
cobol-payment-duplicates
├── src
│   ├── detect_duplicates.cbl
│   └── sample_transactions.csv
└── README.md
```

## Files

- **src/detect_duplicates.cbl**: This file contains the COBOL script that processes the payment transactions and identifies duplicates.
  
- **src/sample_transactions.csv**: This file is a sample CSV containing payment transaction data used by the COBOL script for testing and demonstration purposes. The expected format of the CSV file is as follows:

  ```
  TransactionID,Amount,Date
  1,100.00,2023-01-01
  2,150.00,2023-01-02
  1,100.00,2023-01-01
  ```

## How to Run the COBOL Script

1. Ensure you have a COBOL compiler installed on your Windows machine.
2. Navigate to the `src` directory where the `detect_duplicates.cbl` file is located.
3. Compile the COBOL script using your COBOL compiler.
4. Run the compiled program. It will read the `sample_transactions.csv` file and output any duplicate transactions to the screen.

## Notes

- Make sure the `sample_transactions.csv` file is in the correct format as specified above for the script to function properly.
- Modify the CSV file as needed to test different scenarios of duplicate transactions.