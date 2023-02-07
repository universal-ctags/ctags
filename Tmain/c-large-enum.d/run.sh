# Copyright: 2023 Masatake YAMATO
# License: GPL-2

input=/tmp/c-large-enum.d-input.c
CTAGS="$1 --quiet --options=NONE"

i=0
{
	echo "enum E {"
	while [ $i -lt 32770 ]; do
		printf "X%x," $i
		i=$(($i + 1))
	done
	echo "};"
} > $input

${CTAGS} --sort=no -n --fields= --fields=+"{nth}" -x --_xformat='%{name} %{nth}' "$input" | tail -10
r=$?

rm -f "$input"

exit $r
