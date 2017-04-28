# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

{

echo '# NOSUCHLANG'
${CTAGS} --kinddef-NOSUCHLANG=l,name,description --list-kinds-full=NOSUCHLANG 2>&1

CTAGS="${CTAGS} --langdef=MYTEST"

echo '# define "a"'
${CTAGS} --kinddef-MYTEST=a,aa,aaa --list-kinds-full=MYTEST 2>&1

echo '# no description 1'
${CTAGS} --kinddef-MYTEST=a,aa, --list-kinds-full=MYTEST 2>&1

echo '# no description 2'
${CTAGS} --kinddef-MYTEST=a,aa --list-kinds-full=MYTEST 2>&1

echo '# no name 1'
${CTAGS} --kinddef-MYTEST=a, --list-kinds-full=MYTEST 2>&1

echo '# no name 2'
${CTAGS} --kinddef-MYTEST=a --list-kinds-full=MYTEST 2>&1

echo '# an empty name 1'
${CTAGS} --kinddef-MYTEST=a,,x --list-kinds-full=MYTEST 2>&1

echo '# an empty name 2'
${CTAGS} --kinddef-MYTEST=a,, --list-kinds-full=MYTEST 2>&1

echo '# wrong letter in name 1'
${CTAGS} --kinddef-MYTEST="a,a	x,d" --list-kinds-full=MYTEST 2>&1

echo '# wrong letter in name 2'
${CTAGS} --kinddef-MYTEST="a,a	x," --list-kinds-full=MYTEST 2>&1

echo '# wrong letter in name 3'
${CTAGS} --kinddef-MYTEST="a,a	x" --list-kinds-full=MYTEST 2>&1

echo '# no letter 1'
${CTAGS} --kinddef-MYTEST=,n,d --list-kinds-full=MYTEST 2>&1

echo '# no letter 2'
${CTAGS} --kinddef-MYTEST=,n, --list-kinds-full=MYTEST 2>&1

echo '# no letter 3'
${CTAGS} --kinddef-MYTEST=,n --list-kinds-full=MYTEST 2>&1

echo '# no letter 4'
${CTAGS} --kinddef-MYTEST=, --list-kinds-full=MYTEST 2>&1

echo '# no letter 5'
${CTAGS} --kinddef-MYTEST= --list-kinds-full=MYTEST 2>&1

echo '# wrong letter'
${CTAGS} --kinddef-MYTEST=^ --list-kinds-full=MYTEST 2>&1

echo '# reusing file kind'
${CTAGS} --kinddef-MYTEST=F --list-kinds-full=MYTEST 2>&1

} | sed -e 's/\.exe//'
