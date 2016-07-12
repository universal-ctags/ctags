# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --list-fields | while read K REST; do
    if ! [ "$K" = '-' ]; then
	echo "field: $K"
	${CTAGS} --quiet --options=NONE -x --_xformat="output: %N %${K}" -o - input.py
	echo "status: $?"
	echo
    fi
done
