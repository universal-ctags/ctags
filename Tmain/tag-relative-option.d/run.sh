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
    # Normalize Windows driver letter
    #
    # comment time:   Sat Feb  6 13:11:44 UTC 2021
    # comment author: leleliu008@gmail.com
    #
    # as far as I know, the most widely used unix-like POSIX-compatible environments on Windows are Cygwin and MSYS2. Actually, MSYS2 is a modified fork of Cygwin. They both provide a command-line tool called cygpath which can be used to convert Windows PATH to UNIX path. There exists other unix-like POSIX-compatible environments on Windows, git-for-windows as an example, but they all based on Cygwin or MSYS2.
    if command -v cygpath > /dev/null && command -v awk > /dev/null ; then
        awk '{if ($NF ~ /^[A-Z]:/) { "cygpath "$NF | getline newpath; sub($NF,newpath) } print}'
    else
        # \l is a GNU extension. But only Windows's path match [A-Z]: pattern, Cygwin and MSYS2 use GNU sed.
        sed 's|\([A-Z]\):|/\l\1|'
    fi
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
