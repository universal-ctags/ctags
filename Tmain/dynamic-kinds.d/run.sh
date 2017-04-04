# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1
${CTAGS} --quiet --options=NONE						\
		 --langdef=X --kinddef-X='a,anchor,anchors'	\
		 --kinddef-X='b,batch,batches'				\
		 --list-kinds-full=X
exit $?
