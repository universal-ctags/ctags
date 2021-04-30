# Copyright: 2021 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1

is_feature_available ${CTAGS} json

${CTAGS} --quiet --options=NONE --sort=no --output-format=json input.f
