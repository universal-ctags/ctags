# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_no_coproc ${CTAGS}

chmod u+x ./foo.sh

echo '# default'
${CTAGS} --quiet --options=NONE --with-list-header=no \
         --langdef=foo \
         --xcmd-foo=./foo.sh \
         --list-kinds-full=foo

echo '# disabling'
${CTAGS} --quiet --options=NONE --with-list-header=no \
         --langdef=foo \
         --xcmd-foo=./foo.sh \
	 --kinds-foo='-{variable}{macro}' \
         --list-kinds-full=foo \

echo '# enabling again'
${CTAGS} --quiet --options=NONE --with-list-header=no \
         --langdef=foo \
         --xcmd-foo=./foo.sh \
	 --kinds-foo='-{variable}{macro}' \
	 --kinds-foo='+{macro}{variable}' \
         --list-kinds-full=foo
