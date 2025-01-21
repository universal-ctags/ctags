# Copyright: 2024 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1

V=
# V=valgrind

printf "# %s\n" --extras-X0=+'{iname}'
${V} ${CTAGS} --quiet --options=NONE --options=./x0.ctags --options=./x1.ctags \
	 --extras-X0=+'{iname}' --fields=+'{extras}{language}' -o - input-0.x1

printf "# %s\n" --extras-X0=-'{iname}'
${V} ${CTAGS} --quiet --options=NONE --options=./x0.ctags --options=./x1.ctags \
	 --extras-X0=-'{iname}' --fields=+'{extras}{language}' -o - input-1.x1
