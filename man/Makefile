#
# This is a stand-alone Makefile for Universal-ctags maintainers.
# GNU Make is needed.
#
# This doesn't work with Makefile.am at the top of source tree.
#
#	Copyright (c) 2017, Masatake YAMMATO
#	Copyright (c) 2017, Red Hat, Inc.
#
#	This source code is released for free distribution under the terms
#	of the GNU General Public License version 2 or (at your option) any
#	later version.
#

RST2MAN  = rst2man
RST2HTML = rst2html
RST2PDF  = rst2pdf

# To avoid running configure for quick previewing do:
#
# $ make QUICK=1
#
QUICK    =
CONFIGURE_FLAGS =


.SUFFIXES: .rst .html .pdf .in

SOURCE_FILES = \
	ctags.1.rst.in \
	ctags-optlib.7.rst.in \
	ctags-incompatibilities.7.rst.in \
	\
	$(NULL)

rst_files  = $(basename $(SOURCE_FILES))
man_pages  = $(basename $(rst_files))
html_pages = $(addsuffix .html,$(man_pages))
pdf_pages  = $(addsuffix .pdf,$(man_pages))

#
# pdf files are not created because rst2pdf requires a language name after
#
#   .. code-block::
#
# though rst2man doesn't accept it. docutils must be fixed.
#
all: $(man_pages) $(html_pages) # $(pdf_pages)

%.rst: %.rst.in
ifeq ($(QUICK),)
	(cd ..; ./configure $(CONFIGURE_FLAGS))
else
	cp $< $@
endif

%: %.rst
	$(RST2MAN) $< > $@
%.html: %.rst
	$(RST2HTML) $< > $@
%.pdf: %.rst
	$(RST2PDF) $< > $@

clean:
	rm -f $(man_pages) $(html_pages)
