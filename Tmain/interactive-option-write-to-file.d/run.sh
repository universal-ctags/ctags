#!/bin/sh
# Copyright: 2017 Masatake YAMATO
# Copyright: 2016 Aman Gupta
# License: GPL-2

CTAGS=$1
. ../utils.sh

is_feature_available ${CTAGS} interactive

# It seems that the output format is slightly different between libjansson versions
s()
{
	sed -e s/':"'/': "'/g
}

O=/tmp/ctags-tmain-$$.txt

echo '{"command":"generate-tags", "filename":"test.rb"}' | ${CTAGS} --quiet --options=NONE --_interactive -o ${O} |s

rm -f ${O}
