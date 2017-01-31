# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1"

echo "# default"
${CTAGS} --quiet --options=NONE --with-list-header=no\
		 --list-fields=LdScript

echo "# clear all"
${CTAGS} --quiet --options=NONE --with-list-header=no\
		 --fields-LdScript= --list-fields=LdScript
