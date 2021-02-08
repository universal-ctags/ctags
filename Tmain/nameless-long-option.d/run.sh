# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

{
	${CTAGS} --options=NONE -- 2>&1
	s=$?
} | sed -e 's/.*ctags\(.exe\)\{0,1\}: //' 1>&2
exit $s
