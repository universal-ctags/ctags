# Copyright: 2023 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --langdef=FIELDTEST \
		 --_fielddef-FIELDTEST=strfield,"a field having string value"'{datatype=str}' \
		 --_fielddef-FIELDTEST=boolfield,"a field having boolean value"'{datatype=bool}' \
		 --_fielddef-FIELDTEST=intfield,"a field having integer value"'{datatype=int}' \
		 --_fielddef-FIELDTEST=deffield,"a field that type is not specified" \
		 --list-fields=FIELDTEST
