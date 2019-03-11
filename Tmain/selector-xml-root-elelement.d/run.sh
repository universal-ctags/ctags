# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

is_feature_available ${CTAGS} xpath

${CTAGS} --quiet --options=NONE --print-language input.xml
