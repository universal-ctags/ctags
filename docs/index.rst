.. Universal Ctags documentation master file

#####################################################################
Universal Ctags Hacking Guide
#####################################################################

:Version: Draft
:Authors: Universal Ctags developers
:Web Page: https://ctags.io/

`Universal Ctags`_ generates an index (or tag) file of language objects found in
source files for many popular programming languages. This index makes it easy
for text editors and other tools to locate the indexed items. Universal Ctags
improves on traditional ctags because of its multilanguage support, its ability
for the user to define new languages searched by regular expressions, and its
ability to generate emacs-style TAGS files.

`Universal Ctags`_ has the objective of continuing the development of Darren
Hiebert's `Exuberant-ctags`_ after activity on that project
unfortunately stalled.

Reza Jelveh <reza.jelveh@gmail.com> initially created a personal fork
on GitHub and as interest and participation grew it was decided to
move development to a dedicated GitHub organization.

The goal of this project is to maintain a common/unified working space where
people interested in improving ctags can work together.

This guide is primarily intended for developers. Users should first
consult the ctags.1 man page.

This is a draft document. Proofreading and pull-requests are welcome!


.. _Exuberant-ctags: http://ctags.sourceforge.net/
.. _Universal Ctags: https://github.com/universal-ctags


.. toctree::
	:maxdepth: 2

	building.rst
	man-pages.rst
	parsers.rst
	output-format.rst
	news.rst
	interactive-mode.rst
	reporting.rst
	contributions.rst
	optlib.rst
	extending.rst
	tips.rst
	testing.rst
	other-projects.rst
	developers.rst
