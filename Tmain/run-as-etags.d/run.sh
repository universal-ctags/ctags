# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2
cp $1 $BUILDDIR/etags

$BUILDDIR/etags --_force-quit
r=$?

rm $BUILDDIR/etags

exit $r
