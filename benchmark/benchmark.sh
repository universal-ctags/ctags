#!/usr/bin/env bash
#
# Generates, builds and runs parsers from grammars directory for each git reference supplied as argument.
# Each action is performed multiple times and the times are averaged. Peak memory consumption is also measured.
# First reference is always taken as a "baseline" and others are compared to it. This should allow to compare
# how any given commit affects PackCCs performance.
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
        echo "$TIME ns"
    fi
}

format_mem() {
    MEM="$1"
    if [ -z "$TIME_CMD" ]; then
        echo "??? kB"
    elif [ $((MEM / 1048576)) -gt 10 ]; then
        echo "$((MEM / 1048576)) GB"
    elif [ $((MEM / 1024)) -gt 10 ]; then
        echo "$((MEM / 1024)) MB"
    else
        echo "$MEM kB"
    fi
}

measure() {
    COUNT="$1"
    shift
    MEM=0
    if [ "$TIME_CMD" ]; then
        MEM="$(${TIME_CMD[@]} -f %M "$@" 2>&1 >/dev/null)"
    fi
    START="$(date '+%s%N')"
    for ((i=0; i<COUNT; i++)); do
        "$@" > /dev/null
    done
    END="$(date '+%s%N')"
    TIME=$(( END - START ))
}

benchmark() {
    KEY="${GRAMMAR}_${REF//\//_}"
    NAME="tmp/parser_$KEY"

    echo "Generating $GRAMMAR parser in $REF ($GEN_REPEATS times)..."
    measure "$GEN_REPEATS" "$PACKCC" -o "$NAME" "$GRAMMAR_FILE"
    GEN_TIME["$KEY"]=$TIME
    GEN_MEM["$KEY"]=$MEM
    echo "  Repeated $GEN_REPEATS times in $(format $TIME), peak memory $(format_mem $MEM)"

    echo "Building $GRAMMAR parser in $REF ($BUILD_REPEATS times)..."
    measure "$BUILD_REPEATS" $CC -I. "$NAME".c -o "$NAME"
    BUILD_TIME["$KEY"]=$TIME
    BUILD_MEM["$KEY"]=$MEM
    echo "  Built $BUILD_REPEATS times in $(format $TIME), peak memory $(format_mem $MEM)"

    echo "Running $GRAMMAR parser in $REF ($RUN_REPEATS times)..."
    measure "$RUN_REPEATS" "./$NAME" "$INPUT"
    RUN_TIME["$KEY"]=$TIME
    RUN_MEM["$KEY"]=$MEM
    echo "  Repeated $RUN_REPEATS times in $(format $TIME), peak memory $(format_mem $MEM)"
}

print_table() {
    declare -n RESULTS_TIME="${1}_TIME"
    declare -n RESULTS_MEM="${1}_MEM"
    printf "%-12s" ""
    for REF in "${REFS[@]}"; do
        printf "%-32s" "$REF"
    done
    printf "\n"
    MEMORY=0
    RELATIVE_MEM="???"
    COLOR_MEM=0
    for GRAMMAR in "${GRAMMARS[@]}"; do
        printf "%-12s" "$GRAMMAR"
        for REF in "${REFS[@]}"; do
            KEY="${GRAMMAR}_${REF//\//_}"
            BASE="${GRAMMAR}_${REFS[0]//\//_}"
            TIME="$((${RESULTS_TIME["$KEY"]} / RUN_REPEATS))"
            RELATIVE_TIME="$((100 * RESULTS_TIME["$KEY"] / RESULTS_TIME["$BASE"]))"
            COLOR=$((RELATIVE_TIME == 100 ? 0 : ( RELATIVE_TIME > 100 ? 31 : 32)))
            if [ "$TIME_CMD" ]; then
                MEMORY="${RESULTS_MEM["$KEY"]}"
                RELATIVE_MEM="$((100 * RESULTS_MEM["$KEY"] / RESULTS_MEM["$BASE"]))"
                COLOR_MEM=$((RELATIVE_MEM == 100 ? 0 : ( RELATIVE_MEM > 100 ? 31 : 32)))
            fi
            printf "\033[0;${COLOR}m%-16s\033[0;${COLOR_MEM}m%-16s\033[0m" "$(format $TIME) ($RELATIVE_TIME%)" "$(format_mem $MEMORY) ($RELATIVE_MEM%)"
        done
        printf "\n"
    done
}

print_results() {
    echo
    echo "Generation performance:"
    echo "======================="
    print_table GEN
    echo
    echo "Build performance:"
    echo "=================="
    print_table BUILD
    echo
    echo "Run performance:"
    echo "================"
    print_table RUN
    echo
}

main() {
    set -e

    BENCHDIR="$(cd "$(dirname "$0")" && pwd)"
    ROOTDIR="$BENCHDIR/.."
    declare -a GRAMMARS=()
    declare -A BUILD_TIME GEN_TIME RUN_TIME BUILD_MEM GEN_MEM RUN_MEM

    declare -i GEN_REPEATS="${GEN_REPEATS:-1}"
    declare -i BUILD_REPEATS="${BUILD_REPEATS:-1}"
    declare -i RUN_REPEATS="${RUN_REPEATS:-1}"
    CC="${CC:-cc -O2}"
    REFS=("$@")

    if [[ $# -eq 0 || "$1" =~ -h|--help|--usage ]]; then
        sed -n '3,/^$/s/^#//p' "$0"
        exit 0
    fi

    if which busybox &> /dev/null; then
        TIME_CMD=(busybox time)
    elif which time &> /dev/null; then
        TIME_CMD=("$(which time)")
    else
        echo "NOTE: No time command found, please install GNU time or busybox to measure memory consumption."
        TIME_CMD=""
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
