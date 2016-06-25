# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

: &&
    ${CTAGS} --quiet --options=NONE \
	     --langdef=foo \
	     --map-foo=.qqq \
	     --map-foo='.rb' \
	     --map-foo=+'([Mm]akefile)' \
	     --map-foo=+'(GNUmakefile)' \
	     --map-foo=+.html \
	     --map-foo=+.ppp \
	     --map-foo=+'(RUN.TTT)' \
	     --list-maps | grep '\.qqq\|\.ppp\|\.rb\|\.html\|akefile\|RUN.TTT'
exit $?
