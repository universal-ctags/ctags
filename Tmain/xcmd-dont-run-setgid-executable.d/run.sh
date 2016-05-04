# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_no_coproc ${CTAGS}

X=BACKENDCMD.tmp
touch ${X}
chmod g+xs ${X}

if [ $(ls -l ${X} | sed -e 's/......\(.\).*/\1/') != 's' ]; then
    echo "no setgid on the system"
    exit 77
fi

${CTAGS} --quiet --options=NONE --langdef=foo --xcmd-foo=./${X} --list-kinds | grep foo
S=$?

if [ "$S" = 0 ]; then
    rm $X
fi

exit $S
