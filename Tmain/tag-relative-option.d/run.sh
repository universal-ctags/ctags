# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--quiet --options=NONE -o - -x "
P=$(pwd)

. ../utils.sh

if ! ${CTAGS} $O --help | grep -e --tag-relative | grep --quiet -e always; then
	echo "--tag-relative=always|never is not available on this platform"
	exit ${__SKIP__}
fi

{
    F=./input.c
    ${CTAGS} $O --_xformat="default: $F -> %F"                        $F
    ${CTAGS} $O --_xformat="never:   $F -> %F"  --tag-relative=never  $F
    ${CTAGS} $O --_xformat="no:      $F -> %F"  --tag-relative=no     $F
    ${CTAGS} $O --_xformat="yes:     $F -> %F"  --tag-relative=yes    $F
    ${CTAGS} $O --_xformat="always:  $F -> %F"  --tag-relative=always $F

    F=input.c
    ${CTAGS} $O --_xformat="default: $F -> %F"                        $F
    ${CTAGS} $O --_xformat="never:   $F -> %F"  --tag-relative=never  $F
    ${CTAGS} $O --_xformat="no:      $F -> %F"  --tag-relative=no     $F
    ${CTAGS} $O --_xformat="yes:     $F -> %F"  --tag-relative=yes    $F
    ${CTAGS} $O --_xformat="always:  $F -> %F"  --tag-relative=always $F

    F=${P}/input.c
    ${CTAGS} $O --_xformat="default: $F -> %F"                        $F
    ${CTAGS} $O --_xformat="never:   $F -> %F"  --tag-relative=never  $F
    ${CTAGS} $O --_xformat="no:      $F -> %F"  --tag-relative=no     $F
    ${CTAGS} $O --_xformat="yes:     $F -> %F"  --tag-relative=yes    $F
    ${CTAGS} $O --_xformat="always:  $F -> %F"  --tag-relative=always $F
} | {
	# Normalize driver letter
    sed -e 's|C:|/c|g'
} | {
	# Unescape
	sed -e 's|\\\\|\\|g'
} | {
	# Convert to /
    sed -e 's|\\|/|g'
} | {
	# Convert pwd test environment neutral
    sed -e "s|${P}|/abs|g"
}
