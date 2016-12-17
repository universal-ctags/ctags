#!/bin/sh
#
# Taken from Tmain/interactive-mode.d/run.sh
# Making requests for ctags running in interactive-mode
#
# e.g.
# $ bash misc/mk-interactive-request.sh main/main.c | ./ctags --_interactive -o -
#
filesize()
{
    wc -c < "$1"
}

for f in "$@"; do
	if [ -f "$f" ]; then
		size=$(filesize "$f")
		echo '{"command":"generate-tags", "filename":"'"$f"'", "size":'"$size"'}'
		cat "$f"
	else
		echo "no such file: $f" 1>&2
	fi
done
