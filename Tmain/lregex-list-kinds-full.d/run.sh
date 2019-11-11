# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - \
	 --langdef=foo \
	 --regex-foo='/a/\0/a,xy,x y z/' --kinds-foo=-a --kinds-foo=+a \
	 --regex-foo=/b/\0/b/ --kinds-foo=-b \
	 --regex-foo=/c/\0/c/ \
	 --kinds-foo=-c \
	 --regex-foo=/d/\0/d/ \
	 --machinable \
	 --with-list-header \
	 --list-kinds-full=foo | sort
