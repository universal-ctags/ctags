# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

exit_if_win32 "$CTAGS"

rm -f ./"'"
rm -f ./'"'
rm -f ./'$(ls)'
rm -f ./'a b'

${CTAGS} --quiet --options=NONE -o ./"'" --extras=-pF input.c
${CTAGS} --quiet --options=NONE -o ./'"' --extras=-pF input.c
${CTAGS} --quiet --options=NONE -o ./'$(ls)' --extras=-pF input.c
${CTAGS} --quiet --options=NONE -o ./'a b' --extras=-pF input.c

echo '#' SINGLE QUOTE
if [ -e "'" ]; then
	cat "'"
fi

echo '#' DOUBLE QUOTES
if [ -e '"' ]; then
	cat '"'
fi

echo '#' PROCESS SUBSTITUTION
if [ -e '$(ls)' ]; then
	cat '$(ls)'
fi

echo '#' SPACE
if [ -e 'a b' ]; then
	cat 'a b'
fi

rm -f ./"'"
rm -f ./'"'
rm -f ./'$(ls)'
rm -f ./'a b'
