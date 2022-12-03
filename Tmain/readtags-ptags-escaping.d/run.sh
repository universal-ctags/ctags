#!/bin/sh

# Copyright: 2022 Masatake YAMATO
# License: GPL-2

READTAGS=$3
#V="valgrind --leak-check=full --track-origins=yes -v"
V=

. ../utils.sh

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

# {regular|pseudo}-{field}-{char0{char1}*}.tags
for t in INIT									\
			 regular-name-b_.tags				\
			 regular-name-bb.tags				\
			 regular-name-bt.tags				\
			 regular-input-b_.tags				\
			 regular-input-bb.tags				\
			 regular-input-bt.tags				\
			 regular-kind-b_.tags				\
			 regular-kind-bb.tags				\
			 pseudo-name-b_.tags				\
			 pseudo-name-bb.tags				\
			 pseudo-name-bt.tags				\
			 pseudo-input-b_.tags				\
			 pseudo-input-bb.tags				\
			 pseudo-input-bt.tags				\
		 ; do
	case $t in
		INIT)
			;;
		regular-*)
			echo "# $t"
			${V} ${READTAGS} -Ee -t $t -l || exit 1
			echo
			;;
		pseudo-*)
			echo "# $t"
			${V} ${READTAGS} -Ee -t $t -D || exit 1
			echo
			;;
		*)
			echo "INTERNAL BUG"
			exit 1
			;;
	esac
done
