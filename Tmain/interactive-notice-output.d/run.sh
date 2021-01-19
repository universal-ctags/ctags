# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

# It seems that the output format is slightly different between libjansson versions
s()
{
	sed -e s/':"'/': "'/g
}

if is_feature_available ${CTAGS} json; then
	echo '{"command":"generate-tags", "filename":"input.cst"}' | $CTAGS --options=NONE --map-CTagsSelfTest=.cst --_interactive |s
	echo '{"command":"generate-tags", "filename":"input.cst"}' | $CTAGS --quiet --options=NONE --map-CTagsSelfTest=.cst --_interactive |s
fi
