# Copyright: 2024 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1

V=
# V=valgrind

${V} ${CTAGS} --quiet --options=NONE \
	 \
	 --langdef=NOSUCHLANG'{_foreignLanguage=Kconfig}' \
	 --_extradef-NOSUCHLANG='NOSUCHEXTRA,but this is not the part of Kconfig' \
     --regex-NOSUCHLANG='/^\# (CONFIG_[^ ]+) is not set/\1/c/{_language=Kconfig}{_extra=NOSUCHEXTRA}{exclusive}' \
	 \
	 --_force-quit=0
