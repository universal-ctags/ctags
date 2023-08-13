# Copyright: 2023 Masatake YAMATO
# License: GPL-2

CTAGS=$1

remove_ctags_d=y
if [ -d .ctags.d ]; then
   remove_ctags_d=n
fi

defc_ctags=tmain_defc.ctags
mkdir -p .ctags.d
echo "--langdef=C" > .ctags.d/"$defc_ctags"

"${CTAGS}" --version > /dev/null
status=$?

rm .ctags.d/"$defc_ctags"
if [ "$remove_ctags_d" = y ]; then
	rmdir .ctags.d
fi

exit "$status"
