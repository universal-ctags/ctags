# Copyright: 2026 Masatake YAMATO
# License: GPL-2

CTAGS=$1

gen()
{
	cat <<EOF
int v;
int f(void)
{
	return 0;
}
EOF
}

gen | ${CTAGS} --quiet --options=NONE --oneshot=sort-no.c --sort=no -o -
gen | ${CTAGS} --quiet --options=NONE --oneshot=sort-yes.c --sort=yes -o -
gen | ${CTAGS} --quiet --options=NONE --oneshot=x.c -x
