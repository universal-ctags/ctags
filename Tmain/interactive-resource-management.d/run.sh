# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

if is_feature_available ${CTAGS} json; then
	exit_status_for_input_c $CTAGS NONE      --output-format=json -o -         --sort=no
	exit_status_for_input_c $CTAGS tags.json --output-format=json -o tags.json --sort=no
	exit_status_for_input_c $CTAGS tags.json --output-format=json -o tags.json
	exit_status_for_input_c $CTAGS NONE      --output-format=json -o -
fi
