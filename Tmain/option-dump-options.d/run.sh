# Copyright: 2017 Masatake YAMATO
# License: GPL-2
CTAGS=$1

${CTAGS} \
	--quiet --options=NONE \
	--_dump-options --_force-quit | grep -e '_dump-options'
