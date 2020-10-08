# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

# on "Environment: compiler=msvc, ARCH=x64", we got following error:
#
# C:/projects/ctags/Tmain/run-as-etags.d/etags.exe:
# error while loading shared libraries:
# iconv.dll: cannot open shared object file: No such file or directory
exit_if_win32 "$CTAGS"

etags=$BUILDDIR/etags
cp $1 $etags

$etags --_force-quit
r=$?

rm $etags

exit $r
