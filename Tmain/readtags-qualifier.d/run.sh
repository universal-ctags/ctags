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
echo ';; (and $inherits (#/(^|,)Foo(,|$)/ $inherits) (eq? $kind "class"))' &&
${V} ${READTAGS}  -e -t output.tags -Q '(and $inherits (#/(^|,)Foo(,|$)/ $inherits) (eq? $kind "class"))' -l &&
echo ';; (not ($ "signature"))' &&
${V} ${READTAGS}  -e -t output.tags -Q '(not ($ "signature"))' -l &&
echo ';; (< 1 2)' &&
${V} ${READTAGS}  -e -t output.tags -Q '(< 1 2)' -l &&
echo ';; (and $roles (#/(^|,)superClass(,|$)/ $roles))' &&
${V} ${READTAGS}  -e -n -t roles.tags -Q '(and $roles (#/(^|,)superClass(,|$)/ $roles))' -l &&
echo ';; (substr? (or ($ "roles") "") "super")' &&
${V} ${READTAGS}  -e -t roles.tags -Q '(substr? (or ($ "roles") "") "super")' -l &&
echo ';; (and (eq? $scope-kind "function") (eq? $kind "local"))' &&
${V} ${READTAGS}  -e -t c.tags -Q '(and (eq? $scope-kind "function") (eq? $kind "local"))' -l &&
:
