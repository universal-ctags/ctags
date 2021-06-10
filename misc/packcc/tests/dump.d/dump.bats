#!/usr/bin/env bats

load "$TESTDIR/utils.sh"

@test "Testing dump.d - generation" {
    run "$PACKCC" --debug -o "dump.d/parser" "$ROOTDIR/src/examples/calc.peg" 2>&1
    check_output "dump.d/expected.txt"
}
