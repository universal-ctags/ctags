======================================================================
Relationship between other projects
======================================================================

.. contents:: `Table of contents`
	:depth: 3
	:local:

Other tagging engines
----------------------------------------------------------------------

`Exuberant Ctags <http://ctags.sourceforge.net/>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The origin of Universal Ctags.

`Geany <https://github.com/geany/geany>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Geany is a small and lightweight IDE.
Geany maintains their own tagging engine derived from ctags.
We are looking for the way to merge or share the source code each
other.

Repo

	https://github.com/geany/geany/tree/master/ctags

Geany has created a library out of ctags

  	https://github.com/universal-ctags/ctags/issues/63

Their language parsers have many improvements to various parsers.
Changes known by devs worth backporting:

* Various fixes for D parser (c.c), but currently the code diverges
  from ours to some extent.


They have these additional language parsers:

* `DocBook <https://en.wikipedia.org/wiki/DocBook>`_
* `Vala (c.c) <https://en.wikipedia.org/wiki/Vala_%28programming_language%29>`_

Software using ctags
----------------------------------------------------------------------

`Pygments <https://pygments.org/>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	.. TODO: Is Pygments using ctags? To be move moved to other section?

	Pygments is a generic syntax highlighter.

	It can utilize tags file
	as input for making hyperlinks. However, Pygments just looks
	at names and lines in tags file. scopes and kinds are not
	used.  See `here
	<https://pygments-doc.readthedocs.io/en/latest/formatters/html.html>`_ for
	details.

	As far as I (Masatake YAMATO) tried, using Pygments from ctags
	is not so useful. There are critical gap between ctags and Pygments.
	ctags focuses on identifiers. Pygments focuses on keywords.

`GNU global <https://www.gnu.org/software/global/>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 	GNU global is a source code tagging system.

	I (Masatake YAMATO) don't inspect this much but GNU global uses
	ctags internally.

	A person at GNU global project proposed an extension for the tags file
	format: See `this ticket
	<https://sourceforge.net/p/ctags/mailman/message/30020186/>`_ for details.

	See also `'Source code reading' related sites
	<https://www.gnu.org/software/global/links.html>`_.

`GNU Source-highlight <https://www.gnu.org/software/src-highlite/>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	GNU Source-highlight produces a document with syntax highlighting.

	It can utilize tags file
	as input for making hyperlinks.
	See `Generating References
	<https://www.gnu.org/software/src-highlite/source-highlight.html#Generating-References>`_
	section for details.

	I (Masatake YAMATO) have not tried the feature yet.

`OpenGrok <https://oracle.github.io/opengrok/>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	OpenGrok is a fast and usable source
	code search and cross reference engine.

	I (Masatake YAMATO) don't inspect this much but OpenGrok uses
	ctags internally.

Linux kernel
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	See `linux/scripts/tags.sh <https://elixir.bootlin.com/linux/v5.10.2/source/scripts/tags.sh>`_
	of Linux kernel source tree.
	It utilizes c parser to the utmost limit.

Other interesting ctags repositories
----------------------------------------------------------------------
There are several interesting repo's with ctags around. These are
interesting to integrate in the future.

`VIM-Japan <https://github.com/vim-jp/ctags/>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

VIM-Japan have some interesting things, especially regarding encoding.

`Anjuta <https://gitlab.gnome.org/GNOME/anjuta>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Anjuta DevStudio is a versatile Integrated Development Environment (IDE)
on GNOME Desktop Environment and features a number of advanced
programming facilities.

They did not fork Exuberant Ctags, but they did
natively `include it in Anjuta <https://git.gnome.org/browse/anjuta/tree/plugins/symbol-db/anjuta-tags>`_.
They have made several additions to
their version of it including fairly extensive Vala language support.

`Tagbar <https://github.com/majutsushi/tagbar/>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tagbar is a Vim plugin that provides an easy way to browse the tags of the
current file and get an overview of its structure.

This is `a gold mine of optlibs <https://github.com/majutsushi/tagbar/wiki>`_.

.. include:: tracking.rst
