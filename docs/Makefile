pdf: universal-ctags.pdf
html: universal-ctags.html

universal-ctags.pdf: *.rst *.svg
	rst2pdf universal-ctags.rst -e inkscape
universal-ctags.html: *.rst *.svg
	rst2html universal-ctags.rst > $@
