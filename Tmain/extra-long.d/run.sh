# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo '# resetting'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extra='{subparser}' --list-extra

echo '# enabling 1'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extra=+'{pseudo}' --list-extra

echo '# disabling 1'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extra=-'{fileScope}' --list-extra

echo '# combination'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extra=-'{fileScope}+{inputFile}{reference}' --list-extra

echo '# combination with letters'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extra=-'{fileScope}+p{inputFile}q{reference}.-f' --list-extra
