CTAGS=$1
if ! ${CTAGS} --quiet --options=NONE --help | grep -qe '--gtags' ; then
	echo "--gtags option is not available"
	exit 77
fi
echo "# ctags--quiet --options=NONE -x --format=1 input.scm"
${CTAGS} --quiet --options=NONE -x --format=1 input.scm
echo "# ctags --quiet --options=NONE -x --format=1 --gtags input.scm"
${CTAGS} --quiet --options=NONE -x --format=1 --gtags input.scm
exit $?
