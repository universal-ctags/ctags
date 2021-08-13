#!/bin/sh

# Report the paths causing trouble frequently
echo '##################################################################'
echo '#                The paths and versions for tools                #'
echo '##################################################################'
for t in autoreconf aclocal pkg-config autoconf automake; do
	if type $t; then
		echo '------------------------------------------------------------------'
		$t --version
	fi
	echo '##################################################################'
done

echo '#                        Generating files                        #'
echo '##################################################################'

set -e	# errexit (exit on error)
if [ ! -z "${CI}" ]; then
	set -x	# xtrace (execution trace)
fi

type autoreconf > /dev/null 2>&1 || {
	echo "No autotools (autoconf and automake) found" 1>&2
	exit 1
}
type pkg-config > /dev/null 2>&1 || {
	echo "No pkg-config found" 1>&2
	exit 1
}

autoreconf -vfi
