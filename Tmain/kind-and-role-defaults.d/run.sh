#!/bin/sh

# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1
B0="--quiet --options=NONE"
B1="-o - --fields=+E --language-force=CTagsSelfTest input.cst"
OPT=

OPT=
echo "# $OPT"
${CTAGS} $B0 $OPT $B1
echo

OPT="--kinds-CTagsSelfTest=-e"
echo "# $OPT"
${CTAGS} $B0 $OPT $B1
echo

OPT="--kinds-CTagsSelfTest=-e+d"
echo "# $OPT"
${CTAGS} $B0 $OPT $B1
echo

OPT="--kinds-CTagsSelfTest=+d"
echo "# $OPT"
${CTAGS} $B0 $OPT $B1
echo



OPT="--extras=+r --fields=+r"
echo "# $OPT"
${CTAGS} $B0 $OPT $B1
echo

OPT="--extras=+r --fields=+r --kinds-CTagsSelfTest=-e"
echo "# $OPT"
${CTAGS} $B0 $OPT $B1
echo

OPT="--extras=+r --fields=+r --kinds-CTagsSelfTest=-e+d"
echo "# $OPT"
${CTAGS} $B0 $OPT $B1
echo

OPT="--extras=+r --fields=+r --kinds-CTagsSelfTest=+d"
echo "# $OPT"
${CTAGS} $B0 $OPT $B1
echo

exit $?
