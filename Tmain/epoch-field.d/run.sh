# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

O0=/tmp/ctags-tstamp-$$.c
O1=/tmp/ctags-tstamp-$$.h
O2=/tmp/ctags-tstamp-$$.m

cat > $O0 <<EOF
int main (void)
{
	return 0;
}
EOF

cat > $O1 <<EOF
extern void foo (void);
EOF

cat > $O2 <<EOF
#import <class.h>
EOF

is_json_avaiable()
{
	$1 --quiet --options=NONE --with-list-header=no --list-features | grep -q "json"
}

run()
{
	local o=$1
	local t=$2
	shift 2
	local s

	echo $t
	TZ=UTC+00:00 touch -t '200402291621.42' $o &&
		$CTAGS "$@" --kinds-C= --extras=f --fields=T -o - $o \
			| sed -e 's/.*\(epoch:.*\)/tags:\1/' &&
		$CTAGS "$@" --kinds-C= --extras=f --fields=T -o - -x --_xformat="xref:epoch:%T" $o &&
		{
			if is_json_avaiable $CTAGS; then
				$CTAGS "$@" --kinds-C= --extras=f --fields=T -o - --output-format=json $o \
					| sed -e 's/.*"epoch": \([0-9]*\).*/json:epoch:\1/'
			else
				echo "json:epoch:1078071702"
			fi
		}
	s=$?
	rm $o
	return $s
}

run $O0 ".c file" && run $O1 ".h file" && run $O2 ".m file" -G
exit $?
