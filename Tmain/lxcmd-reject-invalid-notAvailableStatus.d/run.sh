# Copyright: 2016 Thomas Braun
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_no_coproc ${CTAGS}

${CTAGS} --quiet --options=NONE --langdef=ZZZ --xcmd-ZZZ=ls'{notAvailableStatus=x}'
