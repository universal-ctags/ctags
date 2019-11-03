# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

if is_feature_available ${CTAGS} json; then
	exit_status_for_input_c $CTAGS NONE                --output-format=json -o -                   --sort=no
	exit_status_for_input_c $CTAGS $BUILDDIR/tags.json --output-format=json -o $BUILDDIR/tags.json --sort=no
	exit_status_for_input_c $CTAGS $BUILDDIR/tags.json --output-format=json -o $BUILDDIR/tags.json
	exit_status_for_input_c $CTAGS NONE                --output-format=json -o -
fi
