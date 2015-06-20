CTAGS=$1
chmod u+x ./foo.sh
${CTAGS} --quiet --options=NONE -o - \
	 --langdef=foo \
	 --xcmd-foo=./foo.sh \
         --foo-kinds=+a \
	 --foo-kinds=-b \
	 --list-kinds=foo
