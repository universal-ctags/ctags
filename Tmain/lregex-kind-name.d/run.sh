# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

. ../utils.sh

echo2 param: '|(.)|\1|x,name| => acceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,name|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,name,documents| => acceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,name,documents|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,name,0documents| => acceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,name,0documents|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,name,doc uments| => acceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,name,doc uments|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,name0| => acceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,name0|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,name0,documents| => acceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,name0,documents|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,name0,0documents| => acceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,name0,0documents|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,name0,doc uments| => acceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,name0,doc uments|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,0name| => unacceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,0name|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,0name,documents| => unacceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,0name,documents|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,0name,0documents| => unacceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,0name,0documents|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,0name,doc uments| => unacceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,0name,doc uments|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,na me| => unacceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,na me|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,na me,documents| => unacceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,na me,documents|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,na me,0documents| => unacceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,na me,0documents|' --list-kinds-full=x

echo2 param: '|(.)|\1|x,na me,doc uments| => unacceptable'
${CTAGS} --langdef=x --regex-x='|(.)|\1|x,na me,doc uments|' --list-kinds-full=x
