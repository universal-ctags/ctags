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

for etags in '' '-e'; do
	echo "--- multi-byte handling" $(test -n "$etags" && echo "(etags)")

	# as the 7th byte is an inner byte, cutting at 6 and 7 should yield the same result
	${CTAGS} --quiet --options=NONE $etags -o - \
		 --pattern-length-limit=6 \
		 --kinds-python=v  ./input-utf8.py
	${CTAGS} --quiet --options=NONE $etags -o - \
		 --pattern-length-limit=7 \
		 --kinds-python=v  ./input-utf8.py

	${CTAGS} --quiet --options=NONE $etags -o - \
		 --pattern-length-limit=4 \
		 --kinds-python=v  ./input-iso-8859-1.py
done

exit $?
