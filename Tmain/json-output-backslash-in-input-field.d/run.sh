# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

is_feature_available ${CTAGS} json

O="--quiet --options=NONE "

${CTAGS} $O \
         --extras=+p --pseudo-tags=TAG_KIND_SEPARATOR \
         -o - \
		 --output-format=json input.php \
	| grep '"nn"' | grep -v JSON_OUTPUT_VERSION
