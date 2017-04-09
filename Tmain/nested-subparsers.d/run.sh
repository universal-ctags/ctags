# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE  --fields=+lK"

list_kinds()
{
	echo '#'
	echo '#' list kinds$2 $1
	echo '#'
	${CTAGS}  -o - \
			 --options=./event.ctags \
			 --options=./hook.ctags \
			 --options=./plugin.ctags \
			 --list-kinds$2=$1
}

list_kinds C
list_kinds Event
list_kinds Hook
list_kinds Plugin

list_kinds C -full
list_kinds Event -full
list_kinds Hook -full
list_kinds Plugin -full

echo C only
${CTAGS} -o - input.c

echo
echo C + EVENT
${CTAGS} -o - \
		 --options=./event.ctags \
		 input.c


echo
echo C + EVENT + HOOK
${CTAGS}  -o - \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 input.c

echo
echo C + EVENT + HOOK + PLUGIN
${CTAGS}  -o - \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 input.c

echo
echo C + EVENT + HOOK + PLUGIN + UA
${CTAGS}  -o - \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 input.c

echo
echo 'C(disabled)' + EVENT + HOOK + PLUGIN + UA
${CTAGS}  -o - \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 --languages=-C \
		 input.c

echo
echo C + 'EVENT(disabled)' + HOOK + PLUGIN + UA
${CTAGS}  -o - \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 --languages=-Event \
		 input.c

echo
echo C + 'EVENT' + 'HOOK(disabled)' + PLUGIN + UA
${CTAGS}  -o - \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 --languages=-Hook \
		 input.c

echo
echo C + 'EVENT' + 'HOOK' + PLUGIN + 'UA(-v)'
${CTAGS}  -o - \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 --kinds-UnusedAttr=-v \
		 input.c

echo List subparsers of C '(' 'EVENT' + 'HOOK' + PLUGIN + 'UA' ')'
${CTAGS}  \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 --list-subparsers=C

echo List subparsers of C '(' 'EVENT' + 'HOOK' + PLUGIN + 'UA' ')' without the header
${CTAGS} --with-list-header=no \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 --list-subparsers=C

echo List subparsers of C '(' 'EVENT' + 'HOOK' + PLUGIN + 'UA' ')' in machinable
${CTAGS}  \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 --machinable \
		 --list-subparsers=C

echo List subparsers of C '(' 'EVENT' + 'HOOK' + PLUGIN + 'UA' ')' in machinable without the header
${CTAGS} --with-list-header=no \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --options=./plugin.ctags \
		 --options=./unused-attr.ctags \
		 --machinable \
		 --list-subparsers=C
