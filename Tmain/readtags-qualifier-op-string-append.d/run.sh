#!/bin/sh

# Copyright: 2021 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
    skip "no readtags"
fi

if ! ( "${READTAGS}" -h | grep -q -e -S ); then
    skip "no qualifier function in readtags"
fi

r()
{
	echo ';; ' "$1"
	${V} ${READTAGS} -e -t output.tags -Q "$1" -l
}

r '(eq? $scope-name (string-append "ab" $name "cy"))'
r '(eq? $scope-name (concat "ab" "p" "cy"))'
r '(eq? $scope-name (concat (concat "a" "b") $name (concat "c" "y")))'
r '(eq? $scope-name (concat (concat "a" "b" $name) (concat "c" "y")))'
r '(eq? $scope-name (concat (concat (concat "a" "b" $name) (concat "c" "y"))))'
r '((string->regexp (concat (concat (concat "a" "b" $name) (concat "c" "y")))) (or $scope-name ""))'
r '((string->regexp (concat (concat (concat "a" "b" "p") (concat "c" "y")))) (or $scope-name ""))'
r '((printX (string->regexp (concat (concat (concat "a" "b" "p") (concat "c" "y"))))) (or $scope-name ""))'
