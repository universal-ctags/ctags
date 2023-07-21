# Copyright: 2023 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

echo "# --print-language"
for i in input0.pm input1.pm input2.pm input3.pm; do
	${CTAGS} --quiet --options=NONE --print-language $i
done
