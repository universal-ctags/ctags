# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

run_with_format ctags
run_with_format etags
run_with_format xref
