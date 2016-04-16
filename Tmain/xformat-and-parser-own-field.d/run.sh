# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE \
	 -x \
	 --_xformat="%10N %10{sectionMarker}" \
	 input.rst
exit $?
