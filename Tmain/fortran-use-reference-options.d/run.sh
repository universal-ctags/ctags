#!/bin/sh
# License: GPL-2
CTAGS=$1
echo '# default'
"$CTAGS" --quiet --options=NONE --sort=no --fields=+{roles} -o - input.f90 || exit $?
for option in --extras=-r --extras=+r --roles-Fortran.m=-{used} --kinds-Fortran=-m; do
	echo "# $option"
	"$CTAGS" --quiet --options=NONE --sort=no --extras=+r --fields=+{roles} "$option" -o - input.f90 || exit $?
done
