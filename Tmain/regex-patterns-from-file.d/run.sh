# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE \
		 --langdef=TEST'{base=C}' \
		 --kinddef-TEST=t,test,tests \
		 --regex-TEST=@./list.regex \
		 -o - \
		 input.c
