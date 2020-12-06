# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

O=/tmp/ctags-tstamp-$$.c

cat > $O <<EOF
int main (void)
{
	return 0;
}
EOF

is_json_avaiable()
{
	$1 --quiet --options=NONE --with-list-header=no --list-features | grep -q "json"
}

TZ=UTC+00:00 touch -t '200402291621.42' $O &&
	$CTAGS --kinds-C= --extras=f --fields=T -o - $O \
		| sed -e 's/.*\(epoch:.*\)/tags:\1/' &&
	$CTAGS --kinds-C= --extras=f --fields=T -o - -x --_xformat="xref:epoch:%T" $O &&
	{
		if is_json_avaiable $CTAGS; then
			$CTAGS --kinds-C= --extras=f --fields=T -o - --output-format=json $O \
				| sed -e 's/.*"epoch": \([0-9]*\).*/json:epoch:\1/'
		else
			echo "json:epoch:1078071702"
		fi
	}
s=$?
rm $O
exit $s
