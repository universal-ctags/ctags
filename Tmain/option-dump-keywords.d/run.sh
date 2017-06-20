# Copyright: 2017 Masatake YAMATO
# License: GPL-2
CTAGS=$1

${CTAGS} \
	--quiet --options=NONE \
	--_force-initializing --_dump-keywords --_force-quit \
	| grep -i ObjectiveC | sort
