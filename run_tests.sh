#!/bin/bash
# Test suite for COBOL Payment Duplicates Detection System
# Author: AI Assistant
# Description: Comprehensive test suite with coverage reporting

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Create test directories
mkdir -p test_data
mkdir -p test_output
mkdir -p coverage

echo -e "${YELLOW}COBOL Payment Duplicates Test Suite${NC}"
echo "========================================"

# Function to run a test case
run_test() {
    local test_name="$1"
    local test_file="$2"
    local expected_output="$3"
    local should_find_duplicates="$4"
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -e "\n${YELLOW}Running Test: $test_name${NC}"
    echo "----------------------------------------"
    
    # Copy test data file
    cp "$test_file" src/sample_transactions.csv
    
    # Run the program and capture output
    cd src
    ./detect_duplicates > ../test_output/${test_name}.out 2>&1
    cd ..
    
    # Check if output contains expected results
    if [ "$should_find_duplicates" = "true" ]; then
        if grep -q "Duplicate Transactions Found:" test_output/${test_name}.out; then
            echo -e "${GREEN}✓ PASS: Duplicates correctly detected${NC}"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo -e "${RED}✗ FAIL: Expected duplicates but none found${NC}"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            cat test_output/${test_name}.out
        fi
    else
        if grep -q "No duplicate transactions found." test_output/${test_name}.out; then
            echo -e "${GREEN}✓ PASS: No duplicates correctly reported${NC}"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo -e "${RED}✗ FAIL: Expected no duplicates but some found${NC}"
            FAILED_TESTS=$((FAILED_TESTS + 1))
            cat test_output/${test_name}.out
        fi
    fi
    
    # Display test output for review
    echo "Test output:"
    cat test_output/${test_name}.out
}

# Compile the COBOL program
echo -e "\n${YELLOW}Compiling COBOL program...${NC}"
cd src
cobc -x detect_duplicates.cbl -o detect_duplicates
cd ..

# Test Case 1: Original data with known duplicates
echo "Creating test data files..."

cat > test_data/test1_with_duplicates.csv << 'EOF'
transaction_id,amount,date
1,100.00,2023-01-01
2,150.00,2023-01-02
3,200.00,2023-01-03
1,100.00,2023-01-01
2,150.00,2023-01-02
EOF

# Test Case 2: No duplicates
cat > test_data/test2_no_duplicates.csv << 'EOF'
transaction_id,amount,date
1,100.00,2023-01-01
2,150.00,2023-01-02
3,200.00,2023-01-03
4,250.00,2023-01-04
5,300.00,2023-01-05
EOF

# Test Case 3: All duplicates
cat > test_data/test3_all_duplicates.csv << 'EOF'
transaction_id,amount,date
1,100.00,2023-01-01
1,100.00,2023-01-01
1,100.00,2023-01-01
EOF

# Test Case 4: Multiple different duplicates
cat > test_data/test4_multiple_duplicates.csv << 'EOF'
transaction_id,amount,date
1,100.00,2023-01-01
2,150.00,2023-01-02
3,200.00,2023-01-03
1,100.00,2023-01-01
2,150.00,2023-01-02
4,250.00,2023-01-04
3,200.00,2023-01-03
EOF

# Test Case 5: Single record (no duplicates possible)
cat > test_data/test5_single_record.csv << 'EOF'
transaction_id,amount,date
1,100.00,2023-01-01
EOF

# Test Case 6: Empty data file (only header)
cat > test_data/test6_empty_data.csv << 'EOF'
transaction_id,amount,date
EOF

# Test Case 7: Large dataset with some duplicates
cat > test_data/test7_large_dataset.csv << 'EOF'
transaction_id,amount,date
1,100.00,2023-01-01
2,150.00,2023-01-02
3,200.00,2023-01-03
4,250.00,2023-01-04
5,300.00,2023-01-05
6,350.00,2023-01-06
7,400.00,2023-01-07
8,450.00,2023-01-08
9,500.00,2023-01-09
10,550.00,2023-01-10
1,100.00,2023-01-01
5,300.00,2023-01-05
11,600.00,2023-01-11
12,650.00,2023-01-12
EOF

# Run all test cases
run_test "test1_with_duplicates" "test_data/test1_with_duplicates.csv" "" "true"
run_test "test2_no_duplicates" "test_data/test2_no_duplicates.csv" "" "false"
run_test "test3_all_duplicates" "test_data/test3_all_duplicates.csv" "" "true"
run_test "test4_multiple_duplicates" "test_data/test4_multiple_duplicates.csv" "" "true"
run_test "test5_single_record" "test_data/test5_single_record.csv" "" "false"
run_test "test6_empty_data" "test_data/test6_empty_data.csv" "" "false"
run_test "test7_large_dataset" "test_data/test7_large_dataset.csv" "" "true"

# Restore original sample data
cp test_data/test1_with_duplicates.csv src/sample_transactions.csv

# Test Summary
echo -e "\n${YELLOW}Test Summary${NC}"
echo "============="
echo "Total Tests: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed: ${RED}$FAILED_TESTS${NC}"

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "\n${GREEN}🎉 All tests passed!${NC}"
    exit 0
else
    echo -e "\n${RED}❌ Some tests failed. Please review the output above.${NC}"
    exit 1
fi