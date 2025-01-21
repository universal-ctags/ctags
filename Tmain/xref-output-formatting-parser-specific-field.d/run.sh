# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo "#"
echo "# JUST LONG NAME"
echo "#"
${CTAGS} --quiet --options=NONE \
	 -x \
	 --_xformat="%10N %10{sectionMarker}" \
	 input.rst

echo "#"
echo "# LANG, LONG NAME"
echo "#"
${CTAGS} --quiet --options=NONE \
	 -x \
	 --_xformat="%10N %10{reStructuredText.sectionMarker}" \
	 input.rst

echo "#"
echo "# WILDCARD, LONG NAME"
echo "#"
${CTAGS} --quiet --options=NONE \
	 -x \
	 --_xformat="%10N %10{*.sectionMarker}" \
	 input.rst

echo "#"
echo "# WRONG LANGUAGE"
echo "#"
${CTAGS} --quiet --options=NONE \
	 -x \
	 --_xformat="%10N %10{NOSUCHLANG.sectionMarker}" \
	 input.rst

echo "#"
echo "# WRONG FIELD"
echo "#"
${CTAGS} --quiet --options=NONE \
	 -x \
	 --_xformat="%10N %10{NOSUCHFIELD}" \
	 input.rst

echo "#"
echo "# ENABLING COMMON FIELD BY SPECIFYING WILDCARD"
echo "#"
${CTAGS} --quiet --options=NONE \
	 -x \
	 --_xformat="%10N %10{*.language}" \
	 input.rst

echo "#"
echo "# LIST: ENABLING COMMON FIELD BY SPECIFYING WILDCARD"
echo "#"
${CTAGS} --quiet --options=NONE \
	 -x \
	 --_xformat="%10N %10{*.language}" \
	 --list-fields | grep -v NSIS | grep language


exit $?
