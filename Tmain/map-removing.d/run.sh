# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo default map including '*.m'
echo =======================================
${CTAGS} --quiet --options=NONE \
	 --list-maps | grep '\*\.m\>.*$'
echo

echo '[--map-<LANG>]' removing from '*.m' from 'ObjectiveC'
echo =======================================
${CTAGS} --quiet --options=NONE \
	 --map-ObjectiveC=-.m --list-maps | grep '\*\.m\>.*$'
echo

echo '[--map-<LANG>]' adding '*.m' to 'Ada'
echo =======================================
${CTAGS} --quiet --options=NONE \
	 --map-Ada=+.m --list-maps | grep '\*\.m\>.*$'
echo

echo '[--map-<LANG>]' removing from '*.m' from 'ObjectiveC', and adding '*.m' to 'Ada'
echo =======================================
${CTAGS} --quiet --options=NONE \
	 --map-ObjectiveC=-.m --map-Ada=+.m --list-maps | grep '\*\.m\>.*$'
echo

echo '[--map-<LANG>]' guessing parser with adding '*.m' to 'Ada'
echo =======================================
${CTAGS} --quiet --options=NONE \
	 --print-language       \
	 --map-Ada=+.m          \
	 ada.m matlab.m objc.m
echo

echo '[--map-<LANG> --guess-language-eagerly]' guessing parser with adding '*.m' to 'Ada'
echo =======================================
${CTAGS} --quiet --options=NONE   \
	 --print-language         \
	 --map-Ada=+.m            \
	 --guess-language-eagerly \
	 ada.m matlab.m objc.m
echo
