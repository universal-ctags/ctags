# Copyright: 2020 Masatake YAMATO
# License: GPL-2
CTAGS=$1

status ()
{
	echo "status: $1"
} 1>&2

title()
{
    echo
    echo '#'
	for x in "$@"; do
		echo '#' $x
	done
    echo '#'
} 1>&2

run()
{
	title "$@"
	${CTAGS} --quiet --options=NONE "$@" --_force-quit=42
	status $?
}
run '--roles-all=something'

run '--roles-all.*=something'

run '--roles-all.?='

run '--roles-NoSuchLang='

run '--roles-NoSuchLang.*='
run '--roles-NoSuchLang=+{role}'
run '--roles-NoSuchLang=-{role}'

run '--roles-C={role}'
run '--roles-C=+{role}'
run '--roles-C=-{role}'

run '--roles-C.*={role}'
run '--roles-C.*=+{role}'
run '--roles-C.*=-{role}'

run '--roles-C.{header='
run '--roles-C.{file}='
run '--roles-C.{noSuchKind}='
run '--roles-C.{header}x='

run '--roles-C.F='

# run '--roles-C.X='
title "unknown kind letter x"
${CTAGS} --quiet --options=NONE --langdef=MyLang --roles-MyLang.X= --_force-quit=42
status $?

run '--roles-C.fx='
run '--roles-C.?='

run '--roles-C.h={system'
run '--roles-C.h=+{system'
run '--roles-C.h=-{system'
run '--roles-C.h={system}{local'
run '--roles-C.h=+{system}{local'
run '--roles-C.h=-{system}{local'
run '--roles-C.h={system{local}'
run '--roles-C.h=+{system{local}'
run '--roles-C.h=-{system{local}'

run '--roles-C.{header}={system'
run '--roles-C.{header}=+{system'
run '--roles-C.{header}=-{system'
run '--roles-C.{header}={system}{local'
run '--roles-C.{header}=+{system}{local'
run '--roles-C.{header}=-{system}{local'
run '--roles-C.{header}={system{local}'
run '--roles-C.{header}=+{system{local}'
run '--roles-C.{header}=-{system{local}'

run '--roles-C.h={noSuchRole}'
run '--roles-C.h=+{noSuchRole}'
run '--roles-C.h=-{noSuchRole}'
run '--roles-C.h={system}{noSuchRole}'
run '--roles-C.h={noSuchRole}'
run '--roles-C.h=+{system}{noSuchRole}'
run '--roles-C.h=-{system}{noSuchRole}'
run '--roles-C.h=+{system}-{noSuchRole}'
run '--roles-C.h=-{system}+{noSuchRole}'

run '--roles-C.{header}={noSuchRole}'
run '--roles-C.{header}=+{noSuchRole}'
run '--roles-C.{header}=-{noSuchRole}'
run '--roles-C.{header}={system}{noSuchRole}'
run '--roles-C.{header}={noSuchRole}'
run '--roles-C.{header}=+{system}{noSuchRole}'
run '--roles-C.{header}=-{system}{noSuchRole}'
run '--roles-C.{header}=+{system}-{noSuchRole}'
run '--roles-C.{header}=-{system}+{noSuchRole}'

run '--roles-C.h=x{system}'
run '--roles-C.h=+x{system}'
run '--roles-C.h=-x{system}'
run '--roles-C.h=x+{system}'
run '--roles-C.h=+x+{system}'
run '--roles-C.h=-x+{system}'
run '--roles-C.h=x-{system}'
run '--roles-C.h=+x-{system}'
run '--roles-C.h=-x+-system}'

run '--roles-C.{header}=x{system}'
run '--roles-C.{header}=+x{system}'
run '--roles-C.{header}=-x{system}'
run '--roles-C.{header}=x+{system}'
run '--roles-C.{header}=+x+{system}'
run '--roles-C.{header}=-x+{system}'
run '--roles-C.{header}=x-{system}'
run '--roles-C.{header}=+x-{system}'
run '--roles-C.{header}=-x+-system}'
