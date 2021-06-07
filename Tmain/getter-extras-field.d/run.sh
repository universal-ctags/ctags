# Copyright: 2021 Masatake YAMATO
# License: GPL-2

CTAGS=$1

gdb -batch \
	-ex "cd $(pwd)" \
	-ex "run  --verbose --options=NONE -o - --options=x.ctags input.unknown" \
	-ex 'print $pc' \
	-ex "bt" ${CTAGS} \
