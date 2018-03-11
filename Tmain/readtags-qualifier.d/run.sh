#!/bin/sh

# Copyright: 2016 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

if ! ( "${READTAGS}" -h | grep -q -e -Q ); then
    skip "no qualifier function in readtags"
fi

echo ';; (suffix? $name "q")' &&
${V} ${READTAGS} -e -t output.tags -Q '(suffix? $name "q")' -l &&
echo ';; (and (eq? $kind "member") (eq? "Baz" $scope-name))' &&
${V} ${READTAGS} -e -t output.tags -Q '(and (eq? $kind "member") (eq? "Baz" $scope-name))' -l &&
echo ';; (and (eq? $kind "member") (substr? $name "."))' &&
${V} ${READTAGS} -e -t output.tags -Q '(and (eq? $kind "member") (substr? $name "."))' -l &&
echo ';; (and (member "Foo" $inherits) (eq? $kind "class"))' &&
${V} ${READTAGS}  -e -t output.tags -Q '(and (member "Foo" $inherits) (eq? $kind "class"))' -l &&
echo ';; (not ($ "signature"))' &&
${V} ${READTAGS}  -e -t output.tags -Q '(not ($ "signature"))' -l &&
echo ';; (< 1 2)' &&
${V} ${READTAGS}  -e -t output.tags -Q '(< 1 2)' -l &&
echo ';; (member "superClass" $roles)' &&
${V} ${READTAGS}  -e -n -t roles.tags -Q '(member "superClass" $roles)' -l &&
:
