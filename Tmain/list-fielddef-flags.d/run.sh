#!/bin/sh
# Copyright: 2023 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --_list-fielddef-flags
