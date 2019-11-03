# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

${CTAGS} --quiet --options=NONE -o - input1.c input2.c > $BUILDDIR/12.tmp
${CTAGS} --quiet --options=NONE -o - input2.c input1.c > $BUILDDIR/21.tmp

diff $BUILDDIR/12.tmp $BUILDDIR/21.tmp
s=$?

if [ "$s" = 0 ]; then
    rm -f $BUILDDIR/12.tmp
    rm -f $BUILDDIR/21.tmp
fi

exit $s
