# Copyright: 2019 Masatake YAMATO
# License: GPL-2
# The original bug is reported by @elecalion in #2014.

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

tmp="input file.cc"

run()
{
	echo '#'
	echo '#' with $1
	echo '#'

	cp input_file.cc $BUILDDIR/"${tmp}"
	if ! direq $BUILDDIR .; then
		cp input_*.rst $BUILDDIR
		copied=yes
	fi
	(
		cd $BUILDDIR
		"${CTAGS}" --quiet --options=NONE ${1} --output-format=e-ctags \
				   --kinds-c++=+p --fields=+iaSs \
				   -o - \
				   "${tmp}" \
				   input_tab.rst input_space.rst
		rm -f "${tmp}"

		echo "# WITH SCOPE"
		"${CTAGS}" --quiet --options=NONE ${1} --output-format=e-ctags \
				   --fields=+s \
				   -o - \
				   input_scope.rst

		echo "# WITHOUT SCOPE"
		"${CTAGS}" --quiet --options=NONE ${1} --output-format=e-ctags \
				   --fields=-s \
				   -o - \
				   input_scope.rst

		if [ "$copied" = "yes" ]; then
			rm -f input_*.rst
		fi
	)
}

run "--sort=yes"
run "--sort=no"
