# COBOL Payment Duplicates Project

This project contains a COBOL script that reads a sample CSV file containing payment transactions, detects duplicate transactions, and prints the duplicate transactions to the screen.

## Project Structure

```
cobol-payment-duplicates
├── src
│   ├── detect_duplicates.cbl
│   └── sample_transactions.csv
├── test_data/                    # Test data files
├── test_output/                  # Test execution results
├── coverage/                     # Code coverage reports
├── reports/                      # Build summary reports
├── build.sh                      # Build automation script
├── run_tests.sh                  # Test execution script
├── run_coverage.sh              # Coverage analysis script
├── .gitignore                   # Git ignore rules
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

## Prerequisites

- GnuCOBOL compiler (version 3.1.2 or later)
- Bash shell (for running build scripts)
- Git (for version control)

### Installing GnuCOBOL on Ubuntu/Debian:

```bash
sudo apt-get update
sudo apt-get install gnucobol
```

## Quick Start

### Option 1: Complete Build Pipeline (Recommended)

Run the complete build, test, and coverage analysis:

```bash
./build.sh
```

This will:
1. ✅ Compile the COBOL program
2. ✅ Run comprehensive test suite (7 test cases)
3. ✅ Generate code coverage analysis
4. ✅ Create detailed HTML reports

### Option 2: Individual Commands

```bash
# Compile only
./build.sh --compile

# Run tests only
./build.sh --test

# Run coverage analysis only
./build.sh --coverage

# Generate reports only
./build.sh --report

# Clean build artifacts
./build.sh --clean

# Show help
./build.sh --help
```

## Testing

### Test Suite Overview

The project includes a comprehensive test suite with 7 test cases covering various scenarios:

| Test Case | Description | Expected Result |
|-----------|-------------|-----------------|
| test1_with_duplicates | Tests basic duplicate detection | ✅ Detects duplicates |
| test2_no_duplicates | Tests unique transactions | ✅ No duplicates found |
| test3_all_duplicates | Tests all identical transactions | ✅ Detects all as duplicates |
| test4_multiple_duplicates | Tests multiple duplicate groups | ✅ Detects multiple groups |
| test5_single_record | Tests single transaction | ✅ No duplicates possible |
| test6_empty_data | Tests empty data file | ✅ Handles empty input |
| test7_large_dataset | Tests larger dataset | ✅ Scales properly |

### Running Tests Manually

```bash
# Run all tests
./run_tests.sh

# Run individual test (example)
cd src
cobc -x detect_duplicates.cbl -o detect_duplicates
cp ../test_data/test1_with_duplicates.csv sample_transactions.csv
./detect_duplicates
```

## Code Coverage

The project achieves **91% code coverage** across all program paragraphs.

### Coverage Analysis

```bash
# Run coverage analysis
./run_coverage.sh
```

This generates:
- **coverage/coverage_report.html** - Detailed HTML coverage report
- **coverage/combined_coverage.txt** - List of covered paragraphs

### Coverage Metrics

- **Total Paragraphs**: 12
- **Covered Paragraphs**: 11
- **Coverage Percentage**: 91%
- **Missing Coverage**: Only the main entry point paragraph (automatically covered)

## Reports

After running the build pipeline, detailed reports are available:

1. **Build Summary**: `reports/build_summary.html`
   - Overall build status
   - Test results summary
   - Coverage analysis
   - Project information

2. **Coverage Report**: `coverage/coverage_report.html`
   - Detailed paragraph-by-paragraph coverage
   - Execution path analysis
   - Visual coverage indicators

## How to Run the COBOL Script

### Method 1: Using Build System (Recommended)

```bash
./build.sh --compile
cd src
./detect_duplicates
```

### Method 2: Manual Compilation

1. Ensure you have a COBOL compiler installed on your system.
2. Navigate to the `src` directory where the `detect_duplicates.cbl` file is located.
3. Compile the COBOL script:
   ```bash
   cobc -x detect_duplicates.cbl -o detect_duplicates
   ```
4. Run the compiled program:
   ```bash
   ./detect_duplicates
   ```

The program will read the `sample_transactions.csv` file and output any duplicate transactions to the screen.

## Sample Output

```
Payment Duplicate Detection System
======================================
 
Processing Summary:
Total records processed: 007
 
Duplicate Transactions Found:
=============================
Transaction ID: 1          (appears 02 times)
Transaction ID: 2          (appears 02 times)
 
Detailed Duplicate Records:
==========================
ID: 1          | Amount: 100.00     | Date: 2023-01-01
ID: 2          | Amount: 150.00     | Date: 2023-01-02
ID: 2          | Amount: 150.00     | Date: 2023-01-02
ID: 1          | Amount: 100.00     | Date: 2023-01-01
 
Processing completed successfully.
```

## Development Workflow

1. **Make Changes**: Edit the COBOL source code in `src/detect_duplicates.cbl`
2. **Test**: Run `./build.sh --test` to ensure all tests pass
3. **Coverage**: Run `./build.sh --coverage` to verify code coverage
4. **Build**: Run `./build.sh` for complete validation
5. **Review**: Check reports in `reports/` and `coverage/` directories

## Troubleshooting

### Compilation Issues

- Ensure GnuCOBOL is properly installed: `cobc --version`
- Check COBOL syntax and column formatting
- Review compilation warnings in build output

### Test Failures

- Check test output in `test_output/` directory
- Verify CSV input format matches expected structure
- Ensure file permissions are correct

### Coverage Issues

- Verify all test cases are running successfully
- Check that the program compiles without errors
- Review coverage reports for missing execution paths

## Notes

- Make sure the `sample_transactions.csv` file is in the correct format as specified above for the script to function properly.
- Modify the CSV file as needed to test different scenarios of duplicate transactions.
- The build system automatically handles compilation, testing, and reporting.
- All build artifacts are excluded from version control via `.gitignore`.
- The project uses GnuCOBOL 3.1.2 for compilation and execution.