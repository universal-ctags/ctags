# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_no_coproc ${CTAGS}

rm -f ./tags
d=$(basename $(pwd))
cd ..
{
    for r in yes no; do
	${CTAGS} --quiet --options=NONE --tag-relative=$r \
		 --langdef=dog \
		 --langmap=dog:.dog \
		 --xcmd-dog=$d/xcmd-dog \
		 --extra=+f \
		 --fields=+n \
		 -o $d/tags \
		 --append=yes \
		 $d/input.dog
    done
    cat $d/tags | grep -v '^!'
    rm -f $d/tags
}

exit $?
