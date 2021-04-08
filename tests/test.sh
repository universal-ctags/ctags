#!/usr/bin/env bash

generate_bats() {
    skip_all=""
    if [ -f "$1/input.skip.peg" ]; then
        skip_all=$'skip\n'
    fi
    cat <<EOF
#!/usr/bin/env bats

load $TESTDIR/utils.sh

@test "Testing $1 - generation" {
    ${skip_all}test_generate "$1"
}

@test "Testing $1 - compilation" {
    ${skip_all}test_compile "$1"
}
EOF
    for input in "$1"/input*.txt; do
        skip="$skip_all"
        suffix="$(basename "${input/input-/}" .txt)"
        if [[ "$suffix" =~ .skip ]]; then
            skip=true
            suffix="${suffix/.skip/}"
        fi
        if [ "$suffix" = "input" ]; then
            suffix=""
        else
            suffix=" [$suffix]"
        fi
        echo "@test \"Testing $1 - run$suffix\" {"
        [ "$skip" ] && echo "    skip"
        echo "    run_for_input \"$input\""
        echo "}"
    done
}

build() {
    if [ -z "$PACKCC" ]; then
        export PACKCC="$TESTDIR/packcc"
        "${CC:-cc}" -o "$PACKCC" $ROOTDIR/src/packcc.c
    fi
}

clean() {
    rm -f packcc *.d/test.bats *.d/parser{,.c,.h}
}

main() {
    set -e

    export TESTDIR="$(cd "$(dirname "$0")" && pwd)"
    export ROOTDIR="$TESTDIR/.."

    cd "$TESTDIR"
    clean
    build

    for DIR in *.d; do
        # Do not generate test file if the directory already contains some
        ls "$DIR"/*.bats &> /dev/null && continue
        generate_bats "$DIR" > "$DIR/test.bats"
    done

    bats "$@" ./*.d
}

main "$@"
