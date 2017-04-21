# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

exit_status_for_input_c $CTAGS NONE -o - --sort=no
exit_status_for_input_c $CTAGS tags      --sort=no
exit_status_for_input_c $CTAGS tags
exit_status_for_input_c $CTAGS NONE -o -

exit_status_for_input_c $CTAGS NONE -e -o - --sort=no
exit_status_for_input_c $CTAGS TAGS -e      --sort=no
exit_status_for_input_c $CTAGS TAGS -e
exit_status_for_input_c $CTAGS NONE -e -o -

exit_status_for_input_c $CTAGS NONE   -x -o -      --sort=no
exit_status_for_input_c $CTAGS tags-x -x -o tags-x --sort=no
exit_status_for_input_c $CTAGS tags-x -x -o tags-x
exit_status_for_input_c $CTAGS NONE   -x -o -
