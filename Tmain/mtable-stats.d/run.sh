# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

stats=/tmp/ctags-Tmain-$$
${CTAGS} --quiet --options=NONE --options=./args.ctags --totals=extra -o - ./input.foo 2> ${stats}
sed -n -e '/^MTABLE REGEX.*/,$p' ${stats} 1>&2
rm ${stats}
