# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_no_coproc ${CTAGS}

chmod u+x ./foo.sh
echo '# SPECIFYING PARSER'
${CTAGS} --quiet --options=NONE -o - \
	 --langdef=foo \
	 --xcmd-foo=./foo.sh \
         --foo-kinds=+a \
	 --foo-kinds=-b \
	 --_list-kinds-full=foo

echo '# PICKING PARSER with grep'
${CTAGS} --quiet --options=NONE -o - \
	 --langdef=foo \
	 --xcmd-foo=./foo.sh \
         --foo-kinds=+a \
	 --foo-kinds=-b \
	 --_list-kinds-full | grep ^foo
