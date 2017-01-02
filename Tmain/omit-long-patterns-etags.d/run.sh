# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

# strlen ("func95()") => 8
${CTAGS} --quiet --options=NONE --pattern-length-limit=`expr 96 + 8` -e -o - ./input.sh
