# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_no_coproc ${CTAGS}

X=BACKENDCMD.tmp
touch ${X}
chmod u+xs ${X} 
${CTAGS} --quiet --options=NONE --langdef=foo --xcmd-foo=./${X} --list-kinds | grep foo
S=$?

if [ "$S" = 0 ]; then
    rm $X
fi

exit $S
