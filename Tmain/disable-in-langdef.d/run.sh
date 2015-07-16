CTAGS=$1

${CTAGS} --quiet --options=NONE \
	 --langdef=foo'{optional}' --regex-foo=/a/\1/ \
	 --list-languages | grep ^foo

