#!/bin/sh

# Copyright: 2024 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

${V} ${READTAGS} -t output.tags -F '(list (tr $name "_$") #t)' -l

${V} ${READTAGS} -t output.tags -F '(list (tr $name "_$x") #t)' -l
${V} ${READTAGS} -t output.tags -F '(list (tr $name "_") #t)' -l
${V} ${READTAGS} -t output.tags -F '(list (tr $name "") #t)' -l
${V} ${READTAGS} -t output.tags -F '(list (tr $name 2) #t)' -l
