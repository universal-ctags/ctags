#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

echo '!_INPUT_ORDER' &&
${READTAGS} -t output.tags -ne -S '(<or> (<> $input &input) (<> $line &line) (<> $name &name))' -l &&

echo '!_INPUT_ORDER with flipping' &&
${READTAGS} -t output.tags -ne -S '(*- (<or> (<> $input &input) (<> $line &line) (<> $name &name)))' -l &&

echo '!_COMPARING_CODE_SIZE' &&
${READTAGS} -t output.tags -ne -S '(<or> (*- (<> (- (or $end $line) $line) (- (or &end &line) &line))) (<> $input &input) (<> $line &line) (<> $name &name))' -l

echo '!_INPUT_ORDER of structure members (linear search)' &&
${READTAGS} -t output.tags -ne -Q '(eq? $kind "member")' -S '(<or> (<> $input &input) (<> $line &line) (<> $name &name))' -l
