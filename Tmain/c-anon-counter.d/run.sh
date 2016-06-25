# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - input1.c input2.c > ./12.tmp
${CTAGS} --quiet --options=NONE -o - input2.c input1.c > ./21.tmp

diff ./12.tmp ./21.tmp
s=$?

if [ "$s" = 0 ]; then
    rm -f ./12.tmp
    rm -f ./21.tmp
fi

exit $s
