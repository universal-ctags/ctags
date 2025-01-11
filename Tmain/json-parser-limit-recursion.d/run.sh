# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--options=NONE --sort=no -o - --kinds-JSON=-ao"

echo '# depth: 512, items: 1' &&
echo '# depth: 512, items: 1' 1>&2 &&
${CTAGS} --quiet $O input512-one.json &&

echo '# depth: 512, items: 2' &&
echo '# depth: 512, items: 2' 1>&2 &&
${CTAGS} --quiet $O input512-two.json &&

echo '# depth: 513, items: 1' &&
echo '# depth: 513, items: 1' 1>&2 &&
${CTAGS} --quiet $O input513-one.json &&

echo '# depth: 513, items: 2' &&
echo '# depth: 513, items: 2' 1>&2 &&
${CTAGS} --quiet $O input513-two.json &&

echo '# depth: 513, items: 1, NO QUIET' &&
echo '# depth: 513, items: 1, NO QUIET' 1>&2 &&
${CTAGS} $O input513-one.json &&

echo '# depth: 513, items: 2, NO QUIET' &&
echo '# depth: 513, items: 2, NO QUIET' 1>&2 &&
${CTAGS} $O input513-two.json
