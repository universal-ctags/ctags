# Copyright: 2023 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --langdef=FIELDTEST \
		 --_fielddef-FIELDTEST=strfield,"a field having string value"'{datatype=str}' \
		 --_fielddef-FIELDTEST=boolfield,"a field having boolean value"'{datatype=bool}' \
		 --_fielddef-FIELDTEST=intfield,"a field having integer value"'{datatype=int}' \
		 --_fielddef-FIELDTEST=strboolfield,"a field having string value or false"'{datatype=str+bool}' \
		 --_fielddef-FIELDTEST=deffield,"a field that type is not specified" \
		 --list-fields=FIELDTEST && \
! ${CTAGS} --quiet --options=NONE --langdef=FIELDTEST \
  --_fielddef-FIELDTEST=fooA,"unexpected data type"'{datatype=bar}' &&
! ${CTAGS} --quiet --options=NONE --langdef=FIELDTEST \
  --_fielddef-FIELDTEST=fooB,"unexpected data type"'{datatype=int+baz}' &&
! ${CTAGS} --quiet --options=NONE --langdef=FIELDTEST \
  --_fielddef-FIELDTEST=fooC,"unexpected data type"'{datatype=}' &&
! ${CTAGS} --quiet --options=NONE --langdef=FIELDTEST \
  --_fielddef-FIELDTEST=fooD,"unexpected data type"'{datatype=+baz}' &&
! ${CTAGS} --quiet --options=NONE --langdef=FIELDTEST \
  --_fielddef-FIELDTEST=fooE,"unexpected data type"'{datatype=bool+str}'
