CTAGS=$1

${CTAGS} --quiet --options=NONE -o - \
	 --langdef=foo \
	 --regex-foo=/a/\0/a/{optional} --foo-kinds=+a \
	 --regex-foo=/b/\0/b/ --foo-kinds=-b \
	 --regex-foo=/c/\0/c/{optional} \
	 --regex-foo=/d/\0/d/ \
	 --list-kinds=foo

