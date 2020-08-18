# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

if is_feature_available "${CTAGS}" json; then
	${CTAGS} --quiet --options=NONE --output-format=json -o - ./input.cs
fi
