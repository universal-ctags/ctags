# Copyright: 2023 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

echo --map-C++=-.h  --map-C=+.h && \
${CTAGS} --quiet --options=NONE \
		 --map-C++=-.h  --map-C=+.h  \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h &&
echo --map-C=+.h && \
${CTAGS} --quiet --options=NONE \
		 --map-C=+.h  \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h &&
echo --map-C=-.h && \
${CTAGS} --quiet --options=NONE \
		 --map-C++=-.h \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h &&
echo --map-ObjectiveC=-.h && \
${CTAGS} --quiet --options=NONE \
		 --map-ObjectiveC=-.h \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h &&
echo --map-ObjectiveC=-.h --map-C=-.h && \
${CTAGS} --quiet --options=NONE \
		 --map-ObjectiveC=-.h --map-C=-.h  \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h && \
echo --map-ObjectiveC=-.h --map-C++=-.h && \
${CTAGS} --quiet --options=NONE \
		 --map-ObjectiveC=-.h --map-C++=-.h  \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h &&
echo --map-C++=-.h  --map-C=+.h --languages=-ObjectiveC && \
${CTAGS} --quiet --options=NONE \
		 --map-C++=-.h  --map-C=+.h --languages=-ObjectiveC \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h &&
echo --map-C++=-.h  --map-C=+.h --languages=-C++ && \
${CTAGS} --quiet --options=NONE \
		 --map-C++=-.h  --map-C=+.h --languages=-C++ \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h &&
echo --map-C++=-.h  --map-C=+.h --languages=-C && \
${CTAGS} --quiet --options=NONE \
		 --map-C++=-.h  --map-C=+.h --languages=-C \
		 --fields=+'{language}{signature}' \
		 --fields-C=+'{macrodef}' \
		 --kinds-C=d -o - input.h &&
:
