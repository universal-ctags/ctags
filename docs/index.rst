.. Universal Ctags documentation master file

#####################################################################
Universal Ctags Hacking Guide
#####################################################################

:Version: Draft
:Authors: Universal Ctags developers
:Web Page: https://ctags.io/

`Universal Ctags`_ (abbreviated as u-ctags) is a *maintained* implementation of
``ctags``.
``ctags`` generates an index (or tag) file of language objects found in source
files for programming languages.
This index makes it easy for text editors and other tools to locate the indexed
items.

`Exuberant Ctags`_ (e-ctags) maintained by Darren Hiebert, the ancestor of
`Universal Ctags`_, improved traditional ctags with multi-language support, the
ability for the user to define new languages searched by regular expressions
(called optlib in Universal Ctags), and the ability to generate emacs-style TAGS
files.
But the activity of the project unfortunately stalled.

`Universal Ctags`_ has the objective of continuing the development of `Exuberant
Ctags`_.
Reza Jelveh <reza.jelveh@gmail.com> initially created a personal fork of
`Exuberant Ctags`_ on GitHub.
As interest and participation grew, it was decided to move development to a
dedicated project as `Universal Ctags`_.
The goal of this project is to maintain a common/unified working space where
people interested in making ctags better can work together.

Some of the major features of `Universal Ctags`_ are:

* greater number of supported languages
* better language support
    * new extended C/C++ language parser, etc.
* fully extended optlib (a feature to define a new language parser from a
  command line)
* interactive mode (experimental)

The primary documents of `Universal Ctags`_ are man pages. Users should first
consult the :ref:`ctags(1) <ctags(1)>`, and :ref:`other man pages <man-pages>` if
necessary.

This guide, which also includes the man pages, is primarily for developers and
provides additional information to the man pages, including experimental
features.

This is a draft document. Proofreading and pull-requests are welcome!


.. _Exuberant Ctags: http://ctags.sourceforge.net/
.. _Universal Ctags: https://ctags.io/


.. toctree::
	:maxdepth: 2

	building.rst
	man-pages.rst
	parsers.rst
	option-file.rst
	output-format.rst
	running-multi-parsers.rst
	interactive-mode.rst
	news.rst
	optlib.rst
	optscript.rst
	extending.rst
	testing-ctags.rst
	testing-parser.rst
	testing-readtags.rst
	reporting.rst
	contributions.rst
	other-projects.rst
	developers.rst
