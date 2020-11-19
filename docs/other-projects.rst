======================================================================
Relationship between other projects
======================================================================

.. contents:: `Table of contents`
	:depth: 3
	:local:

Geany
----------------------------------------------------------------------
Geany maintains their own tagging engine derived from ctags.
We are looking for the way to merge or share the source code each
other.

Repo

	https://github.com/geany/geany/tree/master/tagmanager/ctags

Geany has created a library out of ctags

  	https://github.com/universal-ctags/ctags/issues/63

Their language parsers have many improvements to various parsers.
Changes known by devs worth backporting:

* Various fixes for D parser (c.c), but currently the code diverges
  from ours to some extent.


They have these additional language parsers:

* `DocBook <https://en.wikipedia.org/wiki/DocBook>`_
* `Ferite (c.c) <https://en.wikipedia.org/wiki/Ferite>`_
* `GLSL (c.c) <https://en.wikipedia.org/wiki/OpenGL_Shading_Language>`_
* `Vala (c.c) <https://en.wikipedia.org/wiki/Vala_%28programming_language%29>`_

.. include:: tracking.rst

Software using ctags
----------------------------------------------------------------------

pygments

	pygments can generate html files. It can utilize tags file
	as input for making hyperlinks. However, pygments just looks
	at names and lines in tags file. scopes and kinds are not
	used.

	As far as I(Masatake YAMATO) tried, using pygments from ctags
	is not so useful. There are critical gap between ctags and pygments.
	ctags focuses on identifiers. pygments focuses on keywords.

GNU global

	I(Masatake YAMATO) don't inspect this much but GNU global uses
	ctags internally.

	A person at GNU global project proposed an extension for the tags file
	format:

	https://sourceforge.net/p/ctags/mailman/message/30020186/

GNU source highlight

	highlight can generate html files. It can utilize tags file
	as input for making hyperlinks.
	https://www.gnu.org/software/src-highlite/source-highlight.html#Generating-References

	I(Masatake YAMATO) have not tried the feature yet.

OpenGrok

	I(Masatake YAMATO) don't inspect this much but OpenGrok uses
	ctags internally.

Linux kernel

	See linux/scripts/tags.sh of Linux kernel source tree.
	It utilizes c parser to the utmost limit.
