# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

exit_if_win32 "$CTAGS"

rm -f $BUILDDIR/"'"
rm -f $BUILDDIR/'"'
rm -f $BUILDDIR/'$(ls)'
rm -f $BUILDDIR/'a b'

${CTAGS} --quiet --options=NONE -o $BUILDDIR/"'" --extras=-pF input.c
${CTAGS} --quiet --options=NONE -o $BUILDDIR/'"' --extras=-pF input.c
${CTAGS} --quiet --options=NONE -o $BUILDDIR/'$(ls)' --extras=-pF input.c
${CTAGS} --quiet --options=NONE -o $BUILDDIR/'a b' --extras=-pF input.c

echo '#' SINGLE QUOTE
if [ -e $BUILDDIR/"'" ]; then
	cat $BUILDDIR/"'"
fi

echo '#' DOUBLE QUOTES
if [ -e $BUILDDIR/'"' ]; then
	cat $BUILDDIR/'"'
fi

echo '#' PROCESS SUBSTITUTION
if [ -e $BUILDDIR/'$(ls)' ]; then
	cat $BUILDDIR/'$(ls)'
fi

echo '#' SPACE
if [ -e $BUILDDIR/'a b' ]; then
	cat $BUILDDIR/'a b'
fi

rm -f $BUILDDIR/"'"
rm -f $BUILDDIR/'"'
rm -f $BUILDDIR/'$(ls)'
rm -f $BUILDDIR/'a b'
