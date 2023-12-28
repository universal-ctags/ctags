# Copyright: 2023 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

remove_ctags_d=y
if [ -d $BUILDDIR/.ctags.d ]; then
   remove_ctags_d=n
fi

defc_ctags=tmain_defc.ctags
mkdir -p $BUILDDIR/.ctags.d
echo "--langdef=C" > $BUILDDIR/.ctags.d/"$defc_ctags"

(cd $BUILDDIR; "${CTAGS}" --version;) > /dev/null
status=$?

rm $BUILDDIR/.ctags.d/"$defc_ctags"
if [ "$remove_ctags_d" = y ]; then
	rmdir $BUILDDIR/.ctags.d
fi

exit "$status"
