#!/bin/sh
# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

column_for_enabled=

if ! column_for_enabled=$(get_column_index $CTAGS --list-extras "ENABLED"); then
	exit $?
fi

$CTAGS --quiet --options=NONE --extras-all= \
	   --with-list-header=no --list-extras \
	| filter_by_column_index $column_for_enabled | sort | uniq
$CTAGS --quiet --options=NONE --extras-all='*' \
	   --with-list-header=no --list-extras \
	| filter_by_column_index $column_for_enabled | sort | uniq
