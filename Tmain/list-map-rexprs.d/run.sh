# Copyright: 2025 Masatake YAMATO
# License: GPL-2

CTAGS=$1

$CTAGS --quiet --options=NONE \
	   --langdef=Something \
	   --map-Something='%\%ESCAPING\%%' \
	   --map-Something=+'%ICASE%i' \
	   --map-Something=+'%TEMP%' \
	   --map-Something=-'%TEMP%' \
	   --map-Something=+'%TEMPi%i' \
	   --map-Something=-'%TEMPi%i' \
	   --list-map-rexprs=Something

$CTAGS --quiet --options=NONE \
	   --langdef=Something \
	   --map-Something='%aExpr%' \
	   --map-Something=+'%\%ESCAPING\%%' \
	   --map-Something=+'%ICASE%i' \
	   --map-Something=+'%TEMP%' \
	   --map-Something=-'%TEMP%' \
	   --map-Something=+'%TEMPi%i' \
	   --map-Something=-'%TEMPi%i' \
	   --list-maps=Something

$CTAGS --quiet --options=NONE \
	   --langdef=Something \
	   --kinddef-Something=t,type,types \
	   --fields=+'{language}' \
	   --regex-Something='/^([a-z]+)[ \t]+tdef;$/\1/t/' \
	   \
	   --map-Something='%something/.*\.c%' \
	   --map-Something=+'%something/.*\.cpp%i' \
	   --map-Something=+'%something/.*\.h%{icase}' \
	   \
	   -x --_xformat='%10N %{language}' \
	   -R something
