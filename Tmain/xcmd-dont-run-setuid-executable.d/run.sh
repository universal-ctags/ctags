CTAGS=$1
X=BACKENDCMD.tmp
touch ${X}
chmod u+xs ${X} 
${CTAGS} --quiet --options=NONE --langdef=foo --xcmd-foo=./${X} --list-kinds | grep foo
S=$?

if [ "$S" = 0 ]; then
    rm $X
fi

exit $S
