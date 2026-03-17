#!/usr/bin/env bash
set -euo pipefail

# Check that benchmark performance hasn't regressed by more than 15%.
# The threshold is set higher than ideal to tolerate measurement noise from
# running benchmarks in parallel with other pre-commit hooks (nix builds, etc.).

BASELINE_FILE=".perf-baseline.csv"
THRESHOLD=15

if [ ! -f "$BASELINE_FILE" ]; then
  echo "No performance baseline found. Running benchmarks to create one..."
  cabal bench --builddir=dist-bench --benchmark-options="+RTS -T -RTS --csv $BASELINE_FILE" 2>&1
  echo "Baseline created at $BASELINE_FILE"
  exit 0
fi

CURRENT_FILE=$(mktemp /tmp/perf-current-XXXXXX.csv)
trap 'rm -f "$CURRENT_FILE"' EXIT

cabal bench --builddir=dist-bench --benchmark-options="+RTS -T -RTS --csv $CURRENT_FILE" 2>&1

# Compare each benchmark: if current time > baseline * (1 + THRESHOLD/100), fail
FAILED=0
while IFS=, read -r name mean _; do
  # Skip header
  if [ "$name" = "Name" ] || [ -z "$name" ]; then
    continue
  fi
  BASELINE_MEAN=$(grep "^${name}," "$BASELINE_FILE" | cut -d, -f2)
  if [ -z "$BASELINE_MEAN" ]; then
    echo "New benchmark: $name (no baseline)"
    continue
  fi

  # Use awk for floating point comparison.
  # The "|| STATUS=$?" idiom prevents set -e from aborting the script when
  # awk exits 1 to signal a regression.
  STATUS=0
  RESULT=$(awk -v cur="$mean" -v base="$BASELINE_MEAN" -v thresh="$THRESHOLD" \
    'BEGIN {
      pct = ((cur - base) / base) * 100;
      if (pct > thresh) { printf "FAIL: %.1f%% slower", pct; exit 1 }
      else { printf "OK: %.1f%%", pct }
    }') || STATUS=$?

  echo "$name: $RESULT"
  if [ $STATUS -ne 0 ]; then
    FAILED=1
  fi
done < "$CURRENT_FILE"

if [ $FAILED -ne 0 ]; then
  echo ""
  echo "FAIL: Performance regression detected (>${THRESHOLD}% slower)."
  exit 1
fi

echo ""
echo "PASS: No significant performance regression."

# Update baseline with current numbers
cp "$CURRENT_FILE" "$BASELINE_FILE"
echo "Updated baseline."
