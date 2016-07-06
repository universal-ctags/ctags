# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - \
	 --kinds-java=f  ./input.java

${CTAGS} --quiet --options=NONE -o - \
	 --pattern-length-limit=10 \
	 --kinds-java=f  ./input.java

${CTAGS} --quiet --options=NONE -o - \
	 --pattern-length-limit=0 \
	 --kinds-java=f  ./input.java

exit $?
