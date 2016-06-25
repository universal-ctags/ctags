# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_no_coproc ${CTAGS}

chmod u+x ./foo.sh
${CTAGS} --quiet --options=NONE -o - \
	 --langdef=foo \
	 --xcmd-foo=./foo.sh \
	 --langmap=foo:+.foo \
         --extra=p \
	 --pseudo-tags=TAG_KIND_DESCRIPTION \
	 input.foo


