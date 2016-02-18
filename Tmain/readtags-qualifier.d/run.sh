#!/bin/sh

# Copyright: 2016 Masatake YAMATO
# License: GPL-2

READTAGS=$4

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
    echo "no readtags"
    eixt 77
fi

if ! ( "${READTAGS}" -h | grep -q -e -Q ); then
    echo "no qualifier function in readtags"
    exit 77
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
:
