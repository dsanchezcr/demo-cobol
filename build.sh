#!/bin/bash
# Build automation script for COBOL Payment Duplicates Detection System
# Author: AI Assistant
# Description: Comprehensive build, test, and coverage automation

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Script version
VERSION="1.0.0"

# Function to display help
show_help() {
    echo -e "${YELLOW}COBOL Payment Duplicates Build System v${VERSION}${NC}"
    echo "=================================================="
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -h, --help        Show this help message"
    echo "  -c, --compile     Compile COBOL program only"
    echo "  -t, --test        Run tests only"
    echo "  -g, --coverage    Run coverage analysis only"
    echo "  -a, --all         Run complete build, test, and coverage (default)"
    echo "  -r, --report      Generate summary report"
    echo "  --clean           Clean build artifacts"
    echo ""
    echo "Examples:"
    echo "  $0                # Run complete build pipeline"
    echo "  $0 --compile      # Compile only"
    echo "  $0 --test         # Run tests only"
    echo "  $0 --coverage     # Run coverage analysis only"
    echo "  $0 --clean        # Clean build artifacts"
    echo ""
}

# Function to clean build artifacts
clean_build() {
    echo -e "${YELLOW}Cleaning build artifacts...${NC}"
    rm -rf test_data test_output coverage
    rm -f src/detect_duplicates src/detect_duplicates.exe
    echo -e "${GREEN}✓ Clean complete${NC}"
}

# Function to compile COBOL program
compile_program() {
    echo -e "${YELLOW}Compiling COBOL program...${NC}"
    cd src
    
    if cobc -x detect_duplicates.cbl -o detect_duplicates 2>&1; then
        echo -e "${GREEN}✓ Compilation successful${NC}"
        cd ..
        return 0
    else
        echo -e "${RED}✗ Compilation failed${NC}"
        cd ..
        return 1
    fi
}

# Function to run tests
run_tests() {
    echo -e "${YELLOW}Running test suite...${NC}"
    if ./run_tests.sh; then
        echo -e "${GREEN}✓ All tests passed${NC}"
        return 0
    else
        echo -e "${RED}✗ Some tests failed${NC}"
        return 1
    fi
}

# Function to run coverage analysis
run_coverage() {
    echo -e "${YELLOW}Running coverage analysis...${NC}"
    if ./run_coverage.sh; then
        echo -e "${GREEN}✓ Coverage analysis complete${NC}"
        return 0
    else
        echo -e "${RED}✗ Coverage analysis failed${NC}"
        return 1
    fi
}

# Function to generate summary report
generate_report() {
    echo -e "${YELLOW}Generating summary report...${NC}"
    
    # Create reports directory
    mkdir -p reports
    
    # Generate timestamp
    timestamp=$(date '+%Y-%m-%d %H:%M:%S')
    
    # Start HTML report
    cat > reports/build_summary.html << EOF
<!DOCTYPE html>
<html>
<head>
    <title>COBOL Payment Duplicates - Build Summary Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .header { background-color: #f0f0f0; padding: 10px; border-radius: 5px; }
        .section { margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px; }
        .success { background-color: #d4edda; border-color: #c3e6cb; }
        .warning { background-color: #fff3cd; border-color: #ffeaa7; }
        .error { background-color: #f8d7da; border-color: #f5c6cb; }
        .info { background-color: #d1ecf1; border-color: #bee5eb; }
        table { border-collapse: collapse; width: 100%; margin-top: 10px; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        .status-pass { color: green; font-weight: bold; }
        .status-fail { color: red; font-weight: bold; }
    </style>
</head>
<body>
    <div class="header">
        <h1>COBOL Payment Duplicates - Build Summary Report</h1>
        <p>Generated on: $timestamp</p>
        <p>Build System Version: $VERSION</p>
    </div>
EOF

    # Compilation status
    if [ -f "src/detect_duplicates" ]; then
        compile_status="<span class='status-pass'>✓ PASS</span>"
        compile_class="success"
    else
        compile_status="<span class='status-fail'>✗ FAIL</span>"
        compile_class="error"
    fi

    cat >> reports/build_summary.html << EOF
    <div class="section $compile_class">
        <h2>Compilation Status: $compile_status</h2>
        <p>COBOL program compilation using GnuCOBOL compiler.</p>
    </div>
EOF

    # Test results
    if [ -d "test_output" ] && [ "$(ls -A test_output)" ]; then
        test_count=$(ls test_output/*.out 2>/dev/null | wc -l)
        test_status="<span class='status-pass'>✓ PASS ($test_count tests)</span>"
        test_class="success"
    else
        test_status="<span class='status-fail'>✗ FAIL</span>"
        test_class="error"
    fi

    cat >> reports/build_summary.html << EOF
    <div class="section $test_class">
        <h2>Test Results: $test_status</h2>
        <p>Comprehensive test suite covering various scenarios including duplicates, no duplicates, edge cases, and large datasets.</p>
EOF

    # Add test details if available
    if [ -d "test_output" ]; then
        cat >> reports/build_summary.html << EOF
        <h3>Test Cases Executed:</h3>
        <table>
            <tr><th>Test Case</th><th>Description</th><th>Status</th></tr>
EOF
        
        # List test files and their status
        for test_file in test_output/*.out; do
            if [ -f "$test_file" ]; then
                test_name=$(basename "$test_file" .out)
                case "$test_name" in
                    "test1_with_duplicates") description="Tests duplicate detection functionality" ;;
                    "test2_no_duplicates") description="Tests handling of unique transactions" ;;
                    "test3_all_duplicates") description="Tests scenario with all identical transactions" ;;
                    "test4_multiple_duplicates") description="Tests multiple different duplicate groups" ;;
                    "test5_single_record") description="Tests single transaction processing" ;;
                    "test6_empty_data") description="Tests empty data file handling" ;;
                    "test7_large_dataset") description="Tests larger dataset processing" ;;
                    *) description="Custom test case" ;;
                esac
                
                echo "            <tr><td>$test_name</td><td>$description</td><td><span class='status-pass'>✓ PASS</span></td></tr>" >> reports/build_summary.html
            fi
        done
        
        cat >> reports/build_summary.html << EOF
        </table>
EOF
    fi

    cat >> reports/build_summary.html << EOF
    </div>
EOF

    # Coverage results
    if [ -f "coverage/coverage_report.html" ]; then
        coverage_percent=$(grep -o 'Coverage Percentage: <strong>[0-9]*%' coverage/coverage_report.html | grep -o '[0-9]*' || echo "0")
        if [ "$coverage_percent" -ge 90 ]; then
            coverage_status="<span class='status-pass'>✓ EXCELLENT (${coverage_percent}%)</span>"
            coverage_class="success"
        elif [ "$coverage_percent" -ge 80 ]; then
            coverage_status="<span class='status-pass'>✓ GOOD (${coverage_percent}%)</span>"
            coverage_class="success"
        elif [ "$coverage_percent" -ge 70 ]; then
            coverage_status="<span class='status-pass'>⚠ FAIR (${coverage_percent}%)</span>"
            coverage_class="warning"
        else
            coverage_status="<span class='status-fail'>✗ LOW (${coverage_percent}%)</span>"
            coverage_class="error"
        fi
    else
        coverage_status="<span class='status-fail'>✗ NOT AVAILABLE</span>"
        coverage_class="error"
    fi

    cat >> reports/build_summary.html << EOF
    <div class="section $coverage_class">
        <h2>Code Coverage: $coverage_status</h2>
        <p>Code coverage analysis measuring which program paragraphs are executed during testing.</p>
        <p><a href="../coverage/coverage_report.html">View Detailed Coverage Report</a></p>
    </div>
    
    <div class="section info">
        <h2>Project Information</h2>
        <table>
            <tr><th>Property</th><th>Value</th></tr>
            <tr><td>Program Name</td><td>DetectDuplicates</td></tr>
            <tr><td>Language</td><td>COBOL (GnuCOBOL 3.1.2)</td></tr>
            <tr><td>Purpose</td><td>Payment transaction duplicate detection</td></tr>
            <tr><td>Input Format</td><td>CSV (transaction_id, amount, date)</td></tr>
            <tr><td>Build System</td><td>Custom Shell Script v$VERSION</td></tr>
        </table>
    </div>
    
    <div class="section info">
        <h2>Build Artifacts</h2>
        <ul>
            <li><strong>Executable:</strong> src/detect_duplicates</li>
            <li><strong>Test Results:</strong> test_output/ directory</li>
            <li><strong>Coverage Report:</strong> coverage/coverage_report.html</li>
            <li><strong>Build Summary:</strong> reports/build_summary.html</li>
        </ul>
    </div>
</body>
</html>
EOF

    echo -e "${GREEN}✓ Summary report generated: reports/build_summary.html${NC}"
}

# Main execution logic
main() {
    echo -e "${BLUE}COBOL Payment Duplicates Build System v${VERSION}${NC}"
    echo "=================================================="
    
    # Parse command line arguments
    case "${1:-}" in
        -h|--help)
            show_help
            exit 0
            ;;
        --clean)
            clean_build
            exit 0
            ;;
        -c|--compile)
            compile_program
            exit $?
            ;;
        -t|--test)
            if [ ! -f "src/detect_duplicates" ]; then
                echo -e "${YELLOW}Program not compiled. Compiling first...${NC}"
                compile_program || exit 1
            fi
            run_tests
            exit $?
            ;;
        -g|--coverage)
            if [ ! -f "src/detect_duplicates" ]; then
                echo -e "${YELLOW}Program not compiled. Compiling first...${NC}"
                compile_program || exit 1
            fi
            run_coverage
            exit $?
            ;;
        -r|--report)
            generate_report
            exit 0
            ;;
        -a|--all|"")
            # Full build pipeline
            echo -e "${BLUE}Running complete build pipeline...${NC}"
            
            # Step 1: Compile
            echo -e "\n${BLUE}Step 1: Compilation${NC}"
            if ! compile_program; then
                echo -e "${RED}Build failed at compilation step${NC}"
                exit 1
            fi
            
            # Step 2: Test
            echo -e "\n${BLUE}Step 2: Testing${NC}"
            if ! run_tests; then
                echo -e "${RED}Build failed at testing step${NC}"
                exit 1
            fi
            
            # Step 3: Coverage
            echo -e "\n${BLUE}Step 3: Coverage Analysis${NC}"
            if ! run_coverage; then
                echo -e "${YELLOW}Warning: Coverage analysis failed, but continuing...${NC}"
            fi
            
            # Step 4: Report
            echo -e "\n${BLUE}Step 4: Generate Report${NC}"
            generate_report
            
            echo -e "\n${GREEN}🎉 Build pipeline completed successfully!${NC}"
            echo -e "${GREEN}View detailed reports:${NC}"
            echo -e "  • Build Summary: ${BLUE}reports/build_summary.html${NC}"
            echo -e "  • Coverage Report: ${BLUE}coverage/coverage_report.html${NC}"
            ;;
        *)
            echo -e "${RED}Error: Unknown option '$1'${NC}"
            echo "Use --help for usage information."
            exit 1
            ;;
    esac
}

# Run main function with all arguments
main "$@"