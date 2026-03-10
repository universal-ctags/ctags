#!/bin/sh

# Copyright: 2026 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

s=0
${READTAGS} -t output.tags -S '(<> 2 1)x' -l > /dev/null
s=`expr $s + $?`

${READTAGS} -t output.tags -S '(<> 2 1)        ' -l > /dev/null
s=`expr $s + $?`

${READTAGS} -t output.tags -S '(<> 2 1)        ; comment' -l > /dev/null
s=`expr $s + $?`

exit $s
