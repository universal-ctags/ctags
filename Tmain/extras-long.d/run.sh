# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo '# resetting'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras='{subparser}' --list-extras

echo '# enabling 1'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=+'{pseudo}' --list-extras

echo '# disabling 1'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}' --list-extras

echo '# combination'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}+{inputFile}{reference}' --list-extras

echo '# combination with letters'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}+p{inputFile}q{reference}-f' --list-extras
