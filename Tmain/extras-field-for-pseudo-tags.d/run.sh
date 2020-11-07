# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

run_ctags()
{
	echo '# option: ' "$@"
	${CTAGS} --quiet --options=NONE \
			 --fields=E --extras=p \
			 --pseudo-tags=-TAG_PROGRAM_VERSION \
			 --pseudo-tags=-TAG_PROC_CWD \
			 $@ \
			 -o - input.c
}

run_ctags --format=1
run_ctags --format=2
