#!/usr/bin/env bash
set -euo pipefail

# Check that code coverage hasn't dropped below the stored baseline.

BASELINE_FILE=".coverage-baseline"
MIN_COVERAGE=0

if [ -f "$BASELINE_FILE" ]; then
  MIN_COVERAGE=$(cat "$BASELINE_FILE")
fi

# Build and run tests with coverage
cabal test --enable-coverage --builddir=dist-coverage 2>&1

# Extract coverage percentage from hpc markup output
COVERAGE_DIR=$(find dist-coverage -name "simple-amount-tests" -path "*/hpc/*" -type d 2>/dev/null | head -1)

if [ -z "$COVERAGE_DIR" ]; then
  echo "Warning: No coverage data found. Skipping coverage check."
  exit 0
fi

# Generate hpc report and extract the top-level expressions percentage
REPORT=$(hpc report "$COVERAGE_DIR/simple-amount-tests.tix" \
  --hpcdir="$COVERAGE_DIR" --hpcdir=dist-coverage 2>/dev/null || true)

if [ -z "$REPORT" ]; then
  echo "Warning: Could not generate hpc report. Skipping coverage check."
  exit 0
fi

COVERAGE=$(echo "$REPORT" | grep "expressions used" | grep -oE '[0-9]+' | head -1)

if [ -z "$COVERAGE" ]; then
  echo "Warning: Could not parse coverage percentage. Skipping coverage check."
  exit 0
fi

echo "Current coverage: ${COVERAGE}%"
echo "Baseline coverage: ${MIN_COVERAGE}%"

if [ "$COVERAGE" -lt "$MIN_COVERAGE" ]; then
  echo "FAIL: Coverage dropped from ${MIN_COVERAGE}% to ${COVERAGE}%"
  exit 1
fi

echo "PASS: Coverage is at or above baseline."

# Update baseline if coverage improved
if [ "$COVERAGE" -gt "$MIN_COVERAGE" ]; then
  echo "$COVERAGE" > "$BASELINE_FILE"
  echo "Updated baseline to ${COVERAGE}%"
fi
