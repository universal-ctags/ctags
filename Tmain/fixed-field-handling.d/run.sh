# Copyright: 2019 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS="$1"

is_feature_available "${CTAGS}" json

CTAGS="${CTAGS} --quiet --options=NONE"

echo '# writer=default'
${CTAGS} --machinable --list-fields | grep '^[NFP]'

list_fields()
{
	local f=$1
	shift

	echo "# writer=$f $o"
	${CTAGS} --output-format=$f --machinable $@ --list-fields | grep '^[NFP]'
}

for f in u-ctags e-ctags etags xref json; do
	list_fields $f
done

for f in xref json; do
	for o in N F P; do
		list_fields $f --fields=-$o
	done
done

for o in N F P; do
	O="--fields=-$o"
	echo "# writer=json $O"
	${CTAGS} --output-format=json $O input.c
	O="--fields=$o"
	echo "# writer=json $O"
	${CTAGS} --output-format=json $O input.c
done

O="--fields="
echo "# writer=json $O"
${CTAGS} --output-format=json $O input.c
