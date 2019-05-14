# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--quiet --options=NONE "

${CTAGS} $O --tag-relative --_force-quit=0
