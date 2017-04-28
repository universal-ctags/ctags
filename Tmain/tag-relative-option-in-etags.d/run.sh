# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
ARGS="--quiet --options=NONE"
O=TAGS.TMP

for x in no yes default; do
    touch ${x}-$O
done

(
    cd indirect

    ${CTAGS} ${ARGS} -e  -o ../no-${O}      --tag-relative=no  src/input.c
    ${CTAGS} ${ARGS} -e  -o ../yes-${O}     --tag-relative=yes src/input.c
    ${CTAGS} ${ARGS} -e  -o ../default-${O}                    src/input.c
)

for x in no yes default; do
    echo '#' ${x}
    # convert path separators in the output
    sed -e 's|\\|/|g' ${x}-$O
    rm ${x}-$O
done

exit $?
