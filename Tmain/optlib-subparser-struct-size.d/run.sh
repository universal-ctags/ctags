#!/bin/sh
# Copyright: 2026 Shivam Maurya
# License: GPL-2
#
# A subparser defined with the optlib {base=...} syntax must be allocated
# large enough for the base parser struct it is cast to.  Regression test
# for #4517: the main part used to allocate a bare `subparser`, and the base
# parser then read a function pointer past the end of it (heap-buffer
# overflow read; SIGSEGV in a non-sanitizer build).

CTAGS=$1

. ../utils.sh

echo '# BibTeX base'
${CTAGS} --quiet --options=NONE \
	'--langdef=Scratch{base=BibTeX}' \
	-o - entry.bib

echo '# Iniconf base'
${CTAGS} --quiet --options=NONE \
	'--langdef=Probe{base=Iniconf}{dedicated}' \
	'--map-Probe=+.probe' \
	'--regex-Probe=/^\[(.*)\]$/\1/s/' \
	-o - section.probe key.probe

echo '# M4 base'
${CTAGS} --quiet --options=NONE \
	'--langdef=ProbeM4{base=M4}' \
	'--map-ProbeM4=+.m4' \
	-o - t.m4
