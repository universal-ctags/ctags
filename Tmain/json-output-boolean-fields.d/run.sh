# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh


if is_feature_available "${CTAGS}" json; then
	${CTAGS} --quiet --options=NONE -o - \
			 --output-format=json \
			 --language-force=CTagsSelfTest input.ctst
fi
