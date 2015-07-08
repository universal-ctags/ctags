CTAGS=$1

chmod u+x ./foo*.sh
for i in $(seq 0 8); do
    $CTAGS --quiet --options=NONE --langdef=foo --xcmd-foo=./foo${i}.sh --list-kinds=foo
done
