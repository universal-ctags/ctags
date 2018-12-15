# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

exit_if_no_case_insensitive_filenames "${CTAGS}"

${CTAGS} --quiet --options=NONE --print-language INPUT.MK
