test_generate () {
    (cd "$BATS_TEST_DIRNAME" && "$PACKCC" "${PACKCC_OPTS[@]}" -o "parser" "${1:-input.peg}")
}

test_compile() {
    ${CC:-cc} -I "$BATS_TEST_DIRNAME" "main.c" -o "$BATS_TEST_DIRNAME/parser" "$@"
}

check_output() {
    diff --strip-trailing-cr -uN "${1/input/expected}" --label "${1/input/expected}" <(echo "$output") --label "output"
}

run_for_input() {
    run timeout 5s "$BATS_TEST_DIRNAME/parser" < "$1"
    check_output "$1"
}

in_header() {
    grep -Fq "$1" "$BATS_TEST_DIRNAME/parser.h"
}

in_source() {
    grep -Fq "$1" "$BATS_TEST_DIRNAME/parser.c"
}

get_line() {
    sed -n "/$1/=" "$BATS_TEST_DIRNAME/$2" | tail -n1
}
