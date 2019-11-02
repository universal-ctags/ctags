# Copyright: 2016 Masatake YAMATO
# License: GPL-2
CTAGS=$1
BUILDDIR=$2

. ../utils.sh

if ! sort --help | grep --quiet GNU; then
    skip "GNU sort is needed to run this test case"
fi

list_languages()
{
    ${CTAGS} --quiet --options=NONE --list-languages
}

list_languages > $BUILDDIR/ll.tmp
list_languages | sort --ignore-case > $BUILDDIR/sorted-ll.tmp
diff -uN $BUILDDIR/ll.tmp $BUILDDIR/sorted-ll.tmp
r=$?
rm $BUILDDIR/ll.tmp $BUILDDIR/sorted-ll.tmp
exit $r
