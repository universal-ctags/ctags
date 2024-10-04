#!/bin/sh

# Copyright: 2019 CCI Europe. Author: Claus Moltke-Leth
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

${V} ${READTAGS} -e -t forward.tags -l &&
${V} ${READTAGS} -e -t backward.tags -l
