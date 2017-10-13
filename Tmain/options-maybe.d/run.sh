# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

d=$(pwd)

${CTAGS} --quiet --options=NONE --options=./NOSUCHFILE -o - input.c

${CTAGS} --quiet --options=NONE --options-maybe=./NOSUCHFILE -o - input-maybe.c
