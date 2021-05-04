# Copyright: 2021 Masatake YAMATO
# License: GPL-2

CTAGS=$1

printf "%s" "--version..."
if ! ${CTAGS} --quiet --options=NONE --langdef=C -o - --version > /dev/null; then
   exit 1
fi
echo "ok"

printf "%s" "input.c..."
if ${CTAGS} --quiet --options=NONE --langdef=C -o - input.c > /dev/null; then
   exit 1
fi
echo "ok"

exit 0
