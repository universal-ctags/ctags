# Copyright: 2016 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1



if ! ( echo '#define foo' > 'a\b.c' ) 2> /dev/null; then
	   skip "unsuitable platform to run this test"
fi

echo '# u-ctags'
${CTAGS} --options=NONE --pseudo-tags=TAG_OUTPUT_MODE --extras=+p --output-format=u-ctags -o - 'a\b.c'

echo '# e-ctags'
${CTAGS} --options=NONE --pseudo-tags=TAG_OUTPUT_MODE --extras=+p --output-format=e-ctags -o - 'a\b.c'

rm 'a\b.c'
