#!/usr/bin/env bats

load "$TESTDIR/utils.sh"

check_uncrustify_version() {
    version="$(uncrustify --version)"
    major="$(echo "$version" | cut -d. -f1 | grep -oE '[0-9]+$')"
    minor="$(echo "$version" | cut -d. -f2)"
    [ "$major" -gt 0 ] || { [ "$major" -eq 0 ] && [ "$minor" -ge 72 ]; }
}

test_style() {
    if ! command -v "uncrustify" &> /dev/null; then
        skip "uncrustify is not installed"
    elif ! check_uncrustify_version &> /dev/null; then
        skip "uncrustify is too old (minimal required version is 0.72.0)"
    else
        run uncrustify -q -c "$TESTDIR/uncrustify.cfg" -f "$1"
        [ "$status" -eq 0 ]
        diff --strip-trailing-cr -uN "$1" --label "$1" <(echo "$output") --label "formatted"
    fi
}

@test "Testing style.d - sources" {
    for file in "$ROOTDIR"/*/*.c; do
        test_style "$file"
    done
}

@test "Testing style.d - generated" {
    test_generate "$ROOTDIR/examples/calc.peg"
    test_style "style.d/parser.h"
    test_style "style.d/parser.c"
}
