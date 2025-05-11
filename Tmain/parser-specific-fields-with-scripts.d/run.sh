# Copyright: 2025 Masatake YAMATO
# License: GPL-2

CTAGS="$1"
CMDLINE="$CTAGS --quiet --options=NONE --options=./testlang.ctags"

. ../utils.sh

is_feature_available "$1" json

echo '### ctags' &&
${CMDLINE} \
		 --param-Testlang.dp=true \
		 -o - input.testlang &&

echo '### xref' &&
${CMDLINE} \
		 --_xformat="%-16N :: str:%{Testlang.str} int:%{Testlang.int} bool:%{Testlang.bool} strbool:%{Testlang.strbool}" \
		 -x input.testlang &&

echo '### json' &&
${CMDLINE} \
		 --output-format=json -o - input.testlang &&

echo '### etags' &&
${CMDLINE} \
		 --output-format=etags -o - input.testlang &&
:
