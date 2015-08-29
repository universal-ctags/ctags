CTAGS=$1

. ../utils.sh
exit_if_no_coproc ${CTAGS}

chmod u+x ./foo.sh
${CTAGS} --quiet --options=NONE -o - \
	 --langdef=foo \
	 --xcmd-foo=./foo.sh \
         --foo-kinds=+a \
	 --foo-kinds=-b \
	 --_list-kinds-full=foo
