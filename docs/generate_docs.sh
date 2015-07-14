#!/bin/bash
#
# generate_docs.sh - Generate rst docs linked together to single pdf or html
#
# Copyright (C) 2015 Zhitao Chen
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
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
FORMAT=$1
NAME=universal-ctags
#set -o errexit
set -o nounset

if [[ $# -ne 1 ]]; then
  exit 1;
fi
CURDIR=`pwd`
TEMPDIR=`mktemp -d` || exit 1
cp * ${TEMPDIR}
pushd ${TEMPDIR}

#translate rst link to include
for rst_file in *.rst; do
  sed -i 's/^`.*<\(.*.rst\)>`_$/.. include:: \1/g' ${rst_file}
done

#add raw::pdf and PageBreak oneColumn after each section
sed -i 's/\(.. include:: .*.rst\)/\1\n.. raw:: pdf\n\n   PageBreak oneColumn/g' readme.rst

#add raw::pdf and PageBreak after content with a blank line
sed -i 's/\(.. section-numbering::\)/\1\n\n.. raw:: pdf\n\n   PageBreak oneColumn/g' readme.rst

if [[ $FORMAT = "pdf" ]]; then
  rst2pdf readme.rst ${NAME}.pdf -e inkscape
  mv ${NAME}.pdf ${CURDIR}
elif [[ $FORMAT = "html" ]]; then
  rst2html readme.rst ${NAME}.html
  mv ${NAME}.html ${CURDIR}
fi

popd
rm -rf ${TEMPDIR}
