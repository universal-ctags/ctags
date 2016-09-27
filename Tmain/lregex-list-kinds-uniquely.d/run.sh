# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE \
		 --langdef=X \
		 --regex-X='/./\0/l,label,labels/' \
		 --regex-X='/./\0/l,label,labels/' \
		 --list-kinds=X
