# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

d=$(pwd)

echo '#' specifying files 1>&2
(
	cd /
	${CTAGS} --options=NONE --optlib-dir=$d/optlib --options=a.ctags --options=b.ctags --options=c.ctags
)

echo '#' specifying a dir 1>&2
(
	cd /
	${CTAGS} --options=NONE --optlib-dir=$d --options=optlib
)

echo '#' reset 1>&2
(
	cd /
	${CTAGS} --options=NONE --optlib-dir= --options=optlib
)
