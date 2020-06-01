#!/bin/sh

# Copyright: 2020 Masatake YAMATO
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

echo '# case sensitive'
${V} ${READTAGS} -t output.tags -Q '(and $signature (#/char,.*,char|int,.*,int/ $signature))' -en -l

echo '# case insensitive: the pattern is lower case'
${V} ${READTAGS} -t output.tags -Q '(and $signature (#/char,.*,char|int,.*,int/i $signature))' -en -l

echo '# case insensitive: the pattern is upper case'
${V} ${READTAGS} -t output.tags -Q '(and $signature (#/CHAR,.*,CHAR|INT,.*,INT/i $signature))' -en -l

echo '# case sensitive (string->regexp)'
${V} ${READTAGS} -t output.tags -Q '(or #f #f (or #f (and $signature ((string->regexp "char,.*,char|int,.*,int") $signature)) #f) #f)' -en -l

echo '# case insensitive: the pattern is lower case (string->regexp)'
${V} ${READTAGS} -t output.tags -Q '(or #f #f (or #f (and $signature ((string->regexp "char,.*,char|int,.*,int" :case-fold) $signature)) #f) #f)' -en -l

echo '# case insensitive: the pattern is upper case (string->regexp)'
${V} ${READTAGS} -t output.tags -Q '(or #f #f (or #f (and $signature ((string->regexp "CHAR,.*,CHAR|INT,.*,INT" :case-fold) $signature)) #f) #f)' -en -l
