#!/bin/sh

# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1

t ()
{
	echo '#' "--all-kinds=$1" 1>&2
	$CTAGS --quiet --options=NONE -o - --all-kinds="$1" --fields=-T input.c
	echo '#' "--kinds-all=$1" 1>&2
	$CTAGS --quiet --options=NONE -o - --kinds-all="$1" --fields=-T input.c
}

echo '#' 1>&2
echo '# unexpected flags after operators ([+-])' 1>&2
echo '#' 1>&2
t '*-a'
t '*-{abc}'
t '*+a'
t '*+{abc}'

t 'F-a'
t 'F-{abc}'
t 'F+a'
t 'F+{abc}'

t '{file}-a'
t '{file}-{abc}'
t '{file}+a'
t '{file}+{abc}'

t '+F-a'
t '+F-{abc}'
t '+F+a'
t '+F+{abc}'

t '+{file}-a'
t '+{file}-{abc}'
t '+{file}+a'
t '+{file}+{abc}'

echo '#' 1>&2
echo '# repeaed operators ([+-])' 1>&2
echo '#' 1>&2
t '*--{file}'
t '*--F'
t '*++{file}'
t '*++F'
t '*+-{file}'
t '*+-F'
t '*-+{file}'
t '*-+F'

t '{file}--F'
t '{file}++F'
t '{file}-+F'
t '{file}+-F'

t '{file}--{file}'
t '{file}++{file}'
t '{file}-+{file}'
t '{file}+-{file}'

t 'F--{file}'
t 'F++{file}'
t 'F-+{file}'
t 'F+-{file}'

t 'F--F'
t 'F++F'
t 'F-+F'
t 'F+-F'

echo '#' 1>&2
echo '# redundant * usage' 1>&2
echo '#' 1>&2
t '-*'
t '+*'
t 'F-*'
t 'F+*'
t '{file}-*'
t '{file}+*'
t '+F-*'
t '-F+*'
t '+{file}-*'
t '-{file}+*'

echo '#' 1>&2
echo '# Just print the parsed file name' 1>&2
echo '#' 1>&2

$CTAGS --quiet --options=NONE -o- --all-kinds=F --extras=+f --fields=-T input.c
$CTAGS --quiet --options=NONE -o- --all-kinds=FF --extras=+f --fields=-T input.c
$CTAGS --quiet --options=NONE -o- --all-kinds=-F+F --extras=+f --fields=-T input.c
$CTAGS --quiet --options=NONE -o- --all-kinds='*' --all-kinds=F --extras=+f --fields=-T input.c

$CTAGS --quiet --options=NONE -o- --kinds-all=F --extras=+f --fields=-T input.c
$CTAGS --quiet --options=NONE -o- --kinds-all=FF --extras=+f --fields=-T input.c
$CTAGS --quiet --options=NONE -o- --kinds-all=-F+F --extras=+f --fields=-T input.c
$CTAGS --quiet --options=NONE -o- --kinds-all='*' --kinds-all=F --extras=+f --fields=-T input.c

echo '#' 1>&2
echo '# The original test cases' 1>&2
echo '#' 1>&2
if ! $CTAGS --quiet --options=NONE --kinds-all=xyz --_force-quit=0; then
	$CTAGS --quiet --options=NONE --all-kinds=abc --_force-quit=0
else
	exit 0
fi
