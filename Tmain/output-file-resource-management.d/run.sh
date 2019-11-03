# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

exit_status_for_input_c $CTAGS NONE           -o -              --sort=no
exit_status_for_input_c $CTAGS $BUILDDIR/tags -o $BUILDDIR/tags --sort=no
exit_status_for_input_c $CTAGS $BUILDDIR/tags -o $BUILDDIR/tags
exit_status_for_input_c $CTAGS NONE           -o -

exit_status_for_input_c $CTAGS NONE           -e -o -              --sort=no
exit_status_for_input_c $CTAGS $BUILDDIR/TAGS -e -o $BUILDDIR/TAGS --sort=no
exit_status_for_input_c $CTAGS $BUILDDIR/TAGS -e -o $BUILDDIR/TAGS
exit_status_for_input_c $CTAGS NONE           -e -o -

exit_status_for_input_c $CTAGS NONE             -x -o -                --sort=no
exit_status_for_input_c $CTAGS $BUILDDIR/tags-x -x -o $BUILDDIR/tags-x --sort=no
exit_status_for_input_c $CTAGS $BUILDDIR/tags-x -x -o $BUILDDIR/tags-x
exit_status_for_input_c $CTAGS NONE             -x -o -
