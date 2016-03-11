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

* HTML reads <h1><h2><h3> tags
* Make has support for targets
* Various fixes for D parser (c.c), but currently the code diverges
  from ours to some extent.


They have these additional language parsers:

* `Abaqus <http://en.wikipedia.org/wiki/Abaqus>`_
* `ActionScript <http://en.wikipedia.org/wiki/ActionScript>`_
* `AsciiDoc <http://en.wikipedia.org/wiki/AsciiDoc>`_
* `DocBook <http://en.wikipedia.org/wiki/DocBook>`_
* `Ferite (c.c) <http://en.wikipedia.org/wiki/Ferite>`_
* `GLSL (c.c) <http://en.wikipedia.org/wiki/OpenGL_Shading_Language>`_
* `Haskell <http://en.wikipedia.org/wiki/Haskell_%28programming_language%29>`_
* `Haxe <http://en.wikipedia.org/wiki/Haxe>`_
* `NSIS <http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System>`_
* `txt2tags <http://en.wikipedia.org/wiki/Txt2tags>`_
* `Vala (c.c) <http://en.wikipedia.org/wiki/Vala_%28programming_language%29>`_

These changes have been merged:

* Fix regex callback match count - https://github.com/universal-ctags/ctags/pull/104
* SQL tags are stored with scopes instead of "tablename.field" - https://github.com/universal-ctags/ctags/pull/100
* Some fixes for D parser
* C++11's enum class/struct support

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

	http://sourceforge.net/p/ctags/mailman/message/30020186/

GNU source highlight

	highlight can generate html files. It can utilize tags file
	as input for making hyperlinks.
	http://www.gnu.org/software/src-highlite/source-highlight.html#Generating-References

	I(Masatake YAMATO) have not tried the feature yet.

OpenGrok

	I(Masatake YAMATO) don't inspect this much but OpenGrok uses
	ctags internally.

Linux kernel

	See linux/scripts/tags.sh of Linux kernel source tree.
	It utilizes c parser to the utmost limit.


