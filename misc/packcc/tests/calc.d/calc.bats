#!/usr/bin/env bats

load "$TESTDIR/utils.sh"

@test "Testing calc.d - generation" {
    test_generate "$ROOTDIR/examples/calc.peg"
}

@test "Testing calc.d - compilation" {
    ${CC:-cc} calc.d/parser.c -o calc.d/parser
}

@test "Testing calc.d - run" {
    run_for_input calc.d/input.txt
}
