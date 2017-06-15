# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--quiet --options=NONE"

echo '# list kinds'
${CTAGS} ${O} --languages=+CTagsSelfTest --list-kinds=CTagsSelfTest | tr '\0' '_' | grep -v seccomp
echo

echo '# list kinds-full'
${CTAGS} ${O} --languages=+CTagsSelfTest --list-kinds-full=CTagsSelfTest | tr '\0' '_' | grep -v seccomp
echo

echo '# +K'
${CTAGS} ${O} --languages=+CTagsSelfTest --language-force=CTagsSelfTest --fields=+K -o - input.x
echo

echo '# +k'
${CTAGS} ${O} --languages=+CTagsSelfTest --language-force=CTagsSelfTest --fields=+k -o - input.x
echo

echo '# +zk'
${CTAGS} ${O} --languages=+CTagsSelfTest --language-force=CTagsSelfTest --fields=+zk -o - input.x
echo

echo '# +Zk'
${CTAGS} ${O} --languages=+CTagsSelfTest --language-force=CTagsSelfTest --fields=+Zk -o - input.x
echo
