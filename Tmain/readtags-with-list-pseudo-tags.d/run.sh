#!/bin/sh

# Copyright: 2023 Masatake YAMATO
# License: GPL-2

READTAGS=$3
#V="valgrind --leak-check=full --track-origins=yes -v"
V=

. ../utils.sh

skip_if_no_readtags "$READTAGS"

echo '# ACTION=LIST'
${V} ${READTAGS} -t ./ptag-sort-no.tags -P -l

echo '# ACTION=NAME'
${V} ${READTAGS} -t ./ptag-sort-yes.tags --with-pseudo-tags - main
