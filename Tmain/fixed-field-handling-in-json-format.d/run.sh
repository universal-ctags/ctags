# Copyright: 2021 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS="$1"

is_feature_available "${CTAGS}" json

CTAGS="${CTAGS} --quiet --options=NONE"

echo '# --output-format=json --fields=+n-P'
${CTAGS} --output-format=json --fields=+n-P -o - input.c

echo '# --fields=+n-P --output-format=json'
${CTAGS} --fields=+n-P --output-format=json -o - input.c
