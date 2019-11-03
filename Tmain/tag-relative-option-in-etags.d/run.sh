# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2
ARGS="--quiet --options=NONE"
O=TAGS.TMP

. ../utils.sh

for x in no yes default; do
    touch $BUILDDIR/${x}-$O
done

if ! direq $BUILDDIR .; then
	cp -r indirect $BUILDDIR
	copied=yes
fi
(
    cd $BUILDDIR/indirect

    ${CTAGS} ${ARGS} -e  -o ../no-${O}      --tag-relative=no  src/input.c
    ${CTAGS} ${ARGS} -e  -o ../yes-${O}     --tag-relative=yes src/input.c
    ${CTAGS} ${ARGS} -e  -o ../default-${O}                    src/input.c
)

for x in no yes default; do
    echo '#' ${x}
    # convert path separators in the output
    sed -e 's|\\|/|g' $BUILDDIR/${x}-$O
    rm $BUILDDIR/${x}-$O
done

if [ "$copied" = "yes" ]; then
	chmod -R u+w $BUILDDIR/indirect
	rm -rf $BUILDDIR/indirect
fi

exit $?
