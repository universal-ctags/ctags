# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE \
	 --langdef=X \
	 --regex-X='/./\0/a,anyobject/' \
	 --regex-X='/./\0/a,anyitem/' \
	 --langdef=Y \
	 --regex-Y='/./\0/b,any' \
	 --regex-Y='/./\0/b,any' \
	 --_force-quit=0 2>&1 | sed -e 's/.*ctags\(.exe\)\{0,1\}: //'
