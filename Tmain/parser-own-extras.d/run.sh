# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--quiet --options=NONE --with-list-header=no"

${CTAGS} --quiet --options=NONE --extras='*' --list-extras=Robot
${CTAGS} ${O} --extras-Robot=-'{whitespaceSwapped}' --list-extras=Robot
${CTAGS} ${O} --extras-Robot=-'{whitespaceSwapped}' --extras-Robot=+'{whitespaceSwapped}' --list-extras=Robot
${CTAGS} ${O} --extras-'*'=-'{whitespaceSwapped}' --list-extras=Robot
${CTAGS} ${O} --extras-'*'=-'{whitespaceSwapped}' --extras-'*'=+'{whitespaceSwapped}' --list-extras=Robot
${CTAGS} ${O} --extras-'*'=-'{whitespaceSwapped}' --extras-Robot=+'{whitespaceSwapped}' --list-extras=Robot
${CTAGS} ${O} --extras-Robot=-'{whitespaceSwapped}' --extras-'*'=+'{whitespaceSwapped}' --list-extras=Robot
