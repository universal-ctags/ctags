# Copyright: 2016 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1



if ! ( echo '#define foo' > 'a\b.c' ) 2> /dev/null; then
	   skip "unsuitable platform to run this test"
fi

echo '# u-ctags'
${CTAGS} --options=NONE --pseudo-tags=TAG_FILE_FORMAT --extras=+p --format=3 -o - 'a\b.c'

echo '# e-ctags'
${CTAGS} --options=NONE --pseudo-tags=TAG_FILE_FORMAT --extras=+p --format=2 -o - 'a\b.c'

rm 'a\b.c'
