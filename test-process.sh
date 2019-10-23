#!/bin/sh

set -ex

# Configuration
MODE=default

case "$1" in
    default|flock|ofd|noop)
        MODE=$1
        ;;
esac

echo "MODE $MODE"

# Build
# cabal v2-build test-process
TEST=$(cabal-plan list-bin test-process)

# Cleanup stuff, which shouldn't be there
rm -rf test-lock
rm -rf test-actual
rm -rf test-expected

# Tests assume there is test-actual file
touch test-actual

# Spawn workers
$TEST $MODE &
PID1=$!

$TEST $MODE &
PID2=$!

$TEST $MODE &
PID3=$!

$TEST $MODE &
PID4=$!

$TEST $MODE &
PID5=$!

# Wait for processes
wait $PID1
wait $PID2
wait $PID3
wait $PID4
wait $PID5

# Expected
cat <<EOF > test-expected
another line
another line
another line
another line
another line
EOF

# Output
cat test-actual
diff -u test-actual test-expected || exit 1

# Cleanup stuff after the test
rm -rf test-lock
rm -rf test-actual
rm -rf test-expected
