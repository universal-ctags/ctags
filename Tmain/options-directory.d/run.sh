# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

${CTAGS} --options=NONE --options=./optlib
