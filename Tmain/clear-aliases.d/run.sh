# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo '# clear' &&
${CTAGS} --quiet --options=NONE --alias-C= --with-list-header=no --list-aliases=C &&

echo '# add' &&
${CTAGS} --quiet --options=NONE --alias-Tcl=+abc --with-list-header=no --list-aliases=Tcl &&

echo '# reset' &&
${CTAGS} --quiet --options=NONE --alias-Tcl=+abc --alias-Tcl=default --with-list-header=no --list-aliases=Tcl

echo '# all clear' &&
${CTAGS} --quiet --options=NONE --alias-all= --with-list-header=no --list-aliases &&

echo '# all reset' &&
${CTAGS} --quiet --options=NONE --alias-Tcl=+abc --alias-all=default --with-list-header=no --list-aliases=Tcl

exit $?
