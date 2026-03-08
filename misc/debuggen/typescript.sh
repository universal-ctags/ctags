#!/bin/sh
#
# debuggen/typescript.sh - Generating decodeToken function for TypeScript parser
#
# Copyright (C) 2026 Masatake YAMATO
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

set -e

. misc/debuggen/libdebuggen.sh

verify_topdir "$0"
verify_ctags

LANGUAGE=typescript
INPUT=./parsers/${LANGUAGE}.c
OUTPUT=./parsers/d-${LANGUAGE}.h


exec > ${OUTPUT}

gen_header "$0" ${LANGUAGE}

gen_enumtbl ${INPUT} eTokenType decodeTokenType TOKEN_
gen_enumtbl ${INPUT} eKeywordId decodeKeywordId KEYWORD_

gen_decodeToken "tokenInfo" "type" decodeTokenType \
				"TOKEN_KEYWORD" "keyword" decodeKeywordId

gen_footer "${LANGUAGE}"
