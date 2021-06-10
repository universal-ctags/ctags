#!/usr/bin/env bats

load "$TESTDIR/utils.sh"

@test "Testing ascii.d - generation" {
    PACKCC_OPTS=("--ascii")
    test_generate
}

@test "Testing ascii.d - check code" {
    ! in_source "pcc_get_char_as_utf32"
}

@test "Testing ascii.d - compilation" {
    test_compile
}
@test "Testing ascii.d - run" {
    run_for_input "ascii.d/input.txt"
}
