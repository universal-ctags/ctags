# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo '# definition'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --langdef=IdealLang --regex-IdealLang='/def (.*)/\1/d,definition,definitions/' \
	 --list-kinds-full=IdealLang

echo '# disabling'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --langdef=IdealLang --regex-IdealLang='/def (.*)/\1/d,definition,definitions/' \
	 --kinds-IdealLang='-{definition}' \
	 --list-kinds-full=IdealLang

echo '# enabling again'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --langdef=IdealLang --regex-IdealLang='/def (.*)/\1/d,definition,definitions/' \
	 --kinds-IdealLang='-{definition}' \
	 --kinds-IdealLang='+{definition}' \
	 --list-kinds-full=IdealLang
