#!/usr/bin/env bash
#
# Generates, builds and runs parsers from grammar directory for each git reference supplied as argument.
# Each action is performed multiple times and the times are averaged. First reference is always
# taken as a "baseline" and others are compared to it. This should allow to compare how any given commit
# affects PackCCs performance.
#
# Usage:
#   ./benchmark.sh <git ref> ...
#
# Environment:
#   CC              Compiler to use, default: "cc -O2"
#   GEN_REPEATS     How many times to generate the parser, default: 10
#   BUILD_REPEATS   How many times to build the parser, default: 5
#   RUN_REPEATS     How many times to run the given parser, default: 20
#
# Example:
#   CC="clang -O3" ./benchmark.sh origin/master 6015afc HEAD

build() {
    echo "Building packcc..."
    $CC -o "$PACKCC" $ROOTDIR/src/packcc.c
}

clean() {
    rm -rf "$BENCHDIR/tmp"
}

format() {
    TIME="$1"
    if [ $((TIME / 1000000000)) -gt 10 ]; then
        echo "$((TIME / 1000000000)) s"
    elif [ $((TIME / 1000000)) -gt 10 ]; then
        echo "$((TIME / 1000000)) ms"
    elif [ $((TIME / 1000)) -gt 10 ]; then
        echo "$((TIME / 1000)) us"
    else
        echo "$((TIME)) ns"
    fi
}

measure() {
    COUNT="$1"
    shift
    START="$(date '+%s%N')"
    for ((i=0; i<COUNT; i++)); do
        "$@"
    done
    END="$(date '+%s%N')"
    TIME=$(( END - START ))
}

run() {
    "$1" < "$2" > /dev/null
}

benchmark() {
    KEY="${GRAMMAR}_${REF//\//_}"
    NAME="tmp/parser_$KEY"

    echo "Generating $GRAMMAR parser in $REF ($GEN_REPEATS times)..."
    measure "$GEN_REPEATS" "$PACKCC" -o "$NAME" "$GRAMMAR_FILE"
    GEN["$KEY"]=$TIME
    echo "  Repeated $GEN_REPEATS times in $(format $TIME)"

    echo "Building $GRAMMAR parser in $REF ($BUILD_REPEATS times)..."
    measure "$BUILD_REPEATS" $CC -I. "$NAME".c -o "$NAME"
    BUILD["$KEY"]=$TIME
    echo "  Built $BUILD_REPEATS times in $(format $TIME)"

    echo "Running $GRAMMAR parser in $REF ($RUN_REPEATS times)..."
    measure "$RUN_REPEATS" run "./$NAME" "$INPUT"
    RUN["$KEY"]=$TIME
    echo "  Repeated $RUN_REPEATS times in $(format $TIME)"
}

print_table() {
    declare -n RESULTS="$1"
    printf "%-12s" ""
    for REF in "${REFS[@]}"; do
        printf "%-16s" "$REF"
    done
    printf "\n"
    for GRAMMAR in "${GRAMMARS[@]}"; do
        printf "%-12s" "$GRAMMAR"
        for REF in "${REFS[@]}"; do
            KEY="${GRAMMAR}_${REF//\//_}"
            BASE="${GRAMMAR}_${REFS[0]//\//_}"
            TIME="$((${RESULTS["$KEY"]} / RUN_REPEATS))"
            RELATIVE="$((100 * RESULTS["$KEY"] / RESULTS["$BASE"]))"
            COLOR=$((RELATIVE == 100 ? 0 : ( RELATIVE > 100 ? 31 : 32)))
            printf "\033[0;${COLOR}m%-16s\033[0m" "$(format $TIME) ($RELATIVE%)"
        done
        printf "\n"
    done
}

print_results() {
    echo
    echo "Generation times:"
    echo "================="
    print_table GEN
    echo
    echo "Build times:"
    echo "============"
    print_table BUILD
    echo
    echo "Run times:"
    echo "=========="
    print_table RUN
}

main() {
    set -e

    BENCHDIR="$(cd "$(dirname "$0")" && pwd)"
    ROOTDIR="$BENCHDIR/.."
    declare -a GRAMMARS=()
    declare -A BUILD=()
    declare -A GEN=()
    declare -A RUN=()

    declare -i GEN_REPEATS="${GEN_REPEATS:-10}"
    declare -i BUILD_REPEATS="${BUILD_REPEATS:-5}"
    declare -i RUN_REPEATS="${RUN_REPEATS:-20}"
    CC="${CC:-cc -O2}"
    REFS=("$@")

    if [[ $# -eq 0 || "$1" =~ -h|--help|--usage ]]; then
        sed -n '3,/^$/s/^#//p' "$0"
        exit 0
    fi

    START_REF="$(git name-rev --name-only HEAD)"
    trap "echo 'Returning to $START_REF...' && git checkout $START_REF" EXIT ERR INT

    cd "$BENCHDIR"
    clean
    mkdir "tmp"
    cp -aL inputs grammars tmp/

    for REF in "${REFS[@]}"; do
        PACKCC="tmp/packcc_${REF//\//_}"
        git checkout "$REF"
        build
        for GRAMMAR_FILE in "tmp/grammars"/*.peg ; do
            GRAMMAR="$(basename "$GRAMMAR_FILE" .peg)"
            [ "$REF" == "${REFS[0]}" ] && GRAMMARS+=("$GRAMMAR")
            INPUT="$(ls "tmp/inputs/$GRAMMAR"*)"
            benchmark
        done
    done

    print_results
}

main "$@"
