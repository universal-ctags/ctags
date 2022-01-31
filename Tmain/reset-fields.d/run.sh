# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1

run_ctags()
{
	printf "# 1:%s 2:%s\n" "$1" "$2"
	${CTAGS} --quiet --options=NONE \
			 $1 \
			 -n \
			 --fields=zKZStse --fields=+a \
			 -o - \
			 $2 \
			 input.h
}

run_ctags &&
run_ctags --output-format=u-ctags "" &&
run_ctags --output-format=etags   "" &&
run_ctags --output-format=xref    "" &&
run_ctags "" --output-format=u-ctags &&
run_ctags "" --output-format=etags   &&
run_ctags "" --output-format=xref    &&
:
