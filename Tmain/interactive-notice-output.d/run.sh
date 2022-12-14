# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

# It seems that the output format is slightly different between libjansson versions
s()
{
	sed -e s/':"'/': "'/g | jdropver
}

if is_feature_available ${CTAGS} json; then
	echo '{"command":"generate-tags", "filename":"input.cst"}' | $CTAGS --options=NONE --pseudo-tags=-TAG_PROGRAM_VERSION \
																		--map-CTagsSelfTest=.cst --_interactive |s
	echo '{"command":"generate-tags", "filename":"input.cst"}' | $CTAGS --quiet --options=NONE --pseudo-tags=-TAG_PROGRAM_VERSION \
																		--map-CTagsSelfTest=.cst --_interactive |s
fi
