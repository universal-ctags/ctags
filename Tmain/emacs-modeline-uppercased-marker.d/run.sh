# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1
$CTAGS --options=NONE --quiet -G --print-language ./macros
