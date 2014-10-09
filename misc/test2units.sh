#!/bin/bash

: ${CTAGS:=./ctags}

function error
{
    local fmt=$1
    shift

    printf "error: ${fmt}\n" "$@"

    exit 1
} 1>&2

function warn
{
    local fmt=$1
    shift

    printf "warn: ${fmt}\n" "$@"
} 1>&2

function checkenv
{
    if ! [[ -x ${CTAGS:?"is not given"}  ]]; then
	error "not executable: %s" "${CTAGS}"
    fi

    if ! [[ -d ./Test ]]; then
	error "cannot find %s directory" "./Test"
    fi

    if ! [[ -d ./Units ]]; then
	error "cannot find %s directory" "./Units"
    fi
}

function help
{
    local status=$1
    shift

    printf "Usage:\n"
    printf "	%s --help|-h\n" $0
    printf "	%s [--dry-run]\n" $0

    exit $status
}

function main
{
    local dry_run

    local t
    local n
    local u
    local x

    case "$1" in
	(-h|--help)
	    help 0
	    ;;
	(--dry-run)
	    dry_run=1
	    ;;
	(-*)
	    help 1 1>&2
	    ;;
    esac


    for t in ./Test/*; do
	if [[ $t =~ .+~ ]]; then
	    warn "ignore backup file: %s" "${t}"
	    continue
	fi

	n=${t##*/}
	u=Units/${n}.t
	if [[ -d  ${u} ]]; then
	    warn "already converted: %s => %s" "${t}" "${u}"
	    continue
	fi

	printf  "converting %s\n" ${n}
	if [[ -n "${dry_run}" ]]; then
	    continue
	fi

	mkdir -p "${u}"
	x=${n##*.}
	cat "${t}" > "${u}"/input."${x}"
	${CTAGS} -o - "${u}"/input."${x}" > "${u}"/expected.tags
    done
}

main "$@"
