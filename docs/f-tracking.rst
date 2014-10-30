Tracking other projects
======================================================================

This is working note for tracking activities other projects,
expecially activity at exuberant-ctags.

I put this as the top of this hacking guide because
I consider tracking activities as the first class fruits
of this project.


exuberant-ctags
----------------------------------------------------------------------

subversion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* status

  Revisions up to <r815> are merged except:

	NOTHING HERE NOW

  (Mon Sep 22 12:41:32 2014 by yamato)

* howto

  ::

      <svn>
      => <git: local exuberant repo>
	 => <git: local fishman repo>


  1. prepare your own fishman repo: a local git repo cloned from github.
     You may know how to do it :)

     ::
    
	$ git clone https://github.com/fishman/ctags.git

  2. prepare exuberant SVN repo: a local git repo clone from exuberant svn tree.

    The original clone is already part of fishman tree.

    To initialize your git repository with the required subversion information do ::

	$ git svn init https://svn.code.sf.net/p/ctags/code/trunk
	$ git update-ref refs/remotes/git-svn refs/remotes/origin/sourceforge

    and then ::

	$ git svn fetch
	$ git svn rebase

    to get the latest changes and reflect it to the local copy.

  3. merge

    TODO

  4. cherry-pick

     4.1. Make a branch at local fishman repo and switch to it.

     4.2. Do cherry-pick like::

	 	$ git cherry-pick -s -x c81a8ce

     You can find commit id on the another terminal
     <git: local exuberant repo>::

	 	$ git log
	 
     or ::
	 
	 	$ git log --oneline

     If conflicts are occurred in cherry-picking, you can
     abort/reset cherry-picking with::

	 $ git reset --hard

     <git: local fishman repo>
      at the branch for picking.

bugs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   <358>  Vim parser: Segmentation fault when reaing empty vim file
	 * directly contributed by the original author of bug report and patch::

	   	e0f854f0100e7a3cb8b959a23d6036e43f6b6c85

	 * it is fixed in sf, too::

	   	5d774f6022a1af71fa5866994699aafce0253085

   <355> Error when parsing empty file (OCaml)
	 * fixed::

	   	02ec2066b5be6b129eba49685bd0b17fef4acfa

   <341> Introducing ctags.conf.d
	 * merged the improved version::

	   	216880c5287e0421d9c49898d983144db61c83aa


`patches <https://sourceforge.net/p/ctags/patches/%d>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tracking the tickets in patch tracker is quite fruitful.
Patches are always there. So it is easy to evaluate the value:)

   [(<]TICKET#[>)] TITLE

	* STATUS

	  + MORE STATUS

   <TICKET#> 

   	means the ticket is closed from the view of fishman tree
   	developers.  We don't have to take time for this ticket.

   (TICKET#) 

   	means the ticket is still opened from the view of fishman
	tree developers.  We don't have to take time for this ticket.

----

   <84> C++11 new using semantics

	* solved by annother implementation::

	      c93e3bfa05b70d7fbc2539454c957eb2169e16b3
	      502355489b1ba748b1a235641bbd512ba6da315e

   <83> New full non-regex PHP parser

	* contributed by the original author

   <82> Support for comments in .ctags files

	* contributed by the original author::

	  	cab4735e4f99ce23c52b78dc879bc06af66796fd

   <81> ocaml parser segfaults on invalid files

	* the bug is not reproduced

   <80> Add support for falcon pl

	* contributed by the original author

   <67> Objective C language parser

	* This is the implementation is we have in fishman tree.

   <65> absoluteFilename uses strcpy on overlapping strings

	* Fixed in exuberant tree, however the ticket is still open::

   		d2bdf505abb7569deae2b50305ea1edce6208557

   <64> Fix strcpy() misuse

	* Fixed in exuberant tree, however the ticket is still open::

		d2bdf505abb7569deae2b50305ea1edce6208557

   <51> Ada support

	* Ada support is now available in fishman tree::

		4b6b4a72f3d2d4ef969d7c650de1829d79f0ea7c

   <38> Ada support

	* Ada support is now available in fishman tree::

		4b6b4a72f3d2d4ef969d7c650de1829d79f0ea7c

   <33> Add basic ObjC support

	* This one is written in regexp.
	* we have better objc parser.

   \(1\) bibtex parser
	
	* Reject because...

	  + the owner of the ticket is anonymous.

	  + the name of patch author is not written explicitly at
	    the header of patch.

	* Alternative

	  https://gist.github.com/ptrv/4576213


devel mailing list (ctags-devel@sourceforge)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    <[Ctags] Shebang with python3 instead of python>
    From: Martin Ueding <dev@ma...> - 2013-01-26 18:36:32

	Added python, python2 and python3 as extensions of
	python parser::

		bb81485205c67617f1b34f61341e60b9e8030502


    <[Ctags-devel] Lack of fnmatch(3) in Windows>
    From: Frank Fesevur <ffes@us...> - 2013-08-24 20:25:47

	There is no fnmatch() in the Windows C library. Therefore
	a string comparison is done in fileNameMatched() in
	strlist.c and patterns are not recognized::

		698bf2f3db692946d2358892d228a864014abc4b


    <Re: [Ctags-devel] WindRes parser>
    From: Frank Fesevur <ffes@unns...> - 2013-08-30 21:23:50

	A parser for Windows Resource files.
	http://en.wikipedia.org/wiki/Resource_%28Windows%29

	::
	
	 	95b4806ba6c006e4b7e72a006700e33c720ab9e7


    ([Ctags-devel] Skip repeat PATH_SEPARATORs in relativeFilename())
    From: Seth Dickson <whefxlr@gm...> - 2013-12-24 04:51:01

	Looks interesting.


Fedora
----------------------------------------------------------------------

Some patches are maintained in ctags package of Fedora.
Inventory of patches are
http://pkgs.fedoraproject.org/cgit/ctags.git/tree/ctags.spec

<ctags-5.7-destdir.patch>

	This patch was merged in fishman ctags git tree::

		d4b5972427a46cbdcbfb050a944cf62b300676be

<ctags-5.7-segment-fault.patch>

	This patch was merged in fishman ctags git tree::

		8cc2b482f6c7257c5151893a6d02b8c79851fedd

(ctags-5.8-cssparse.patch)

	Not in fishman tree.

	The reproducer is attached to following page:
	https://bugzilla.redhat.com/show_bug.cgi?id=852101

	However, fishman-ctags doesn't reproduce with it.

	I, Masatake YAMATO, read the patch.  However, I don't
	understand the patch.  

<ctags-5.8-css.patch>

	This patch was merged in fishman ctags git tree::

		80c1522a36df3ba52b8b7cd7f5c79d5c30437a63

<ctags-5.8-memmove.patch>

	This patch was merged in exuberant ctags svn tree.
	As the result this patch is in fishman tree::

		d2bdf505abb7569deae2b50305ea1edce6208557

<ctags-5.8-ocaml-crash.patch>

	This patch was merged in exuberant ctags svn tree.
	As the result this patch is in fishman tree::

		ddb29762b37d60a875252dcc401de0b7479527b1

<ctags-5.8-format-security.patch>

	This patch was merged in exuberant ctags svn tree.
	As the result this patch is in fishman tree::

		2f7a78ce21e4156ec3e63c821827cf1d5680ace8

Debian
----------------------------------------------------------------------

Some patches are maintained in ctags package of Debian.
Inventory of patches are
http://anonscm.debian.org/cgit/users/cjwatson/exuberant-ctags.git/tree/debian/patches/series

(python-disable-imports.patch)

	Not in fishman tree.
	
	I don't want to merge this patch. I think ctags should extract
	as much as possible informatoin from input source code.
	The user has responsibility to filter out the noise.
	The definition of noise is upto the user.

<vim-command-loop.patch>

	This patch was merged as an alternative for 
	7fb36a2f4690374526e9e7ef4f1e24800b6914ec
	
	Discussed on https://github.com/fishman/ctags/issues/74

	::
	
	   	e59325a576e38bc63b91abb05a5a22d2cef25ab7


Other interesting ctags repositories
----------------------------------------------------------------------
There are several interesting repo's with ctags around. These are
interesting to integrate in the future.


Geany
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Repo

	https://github.com/geany/geany/tree/master/tagmanager/ctags

Geany has created a library out of ctags

  	https://github.com/fishman/ctags/issues/63

Their language parsers have many improvements to various parsers.
Changes known by fishman devs worth backporting:

* Make has support for targets
* SQL tags are stored with scopes instead of "tablename.field"


They have a these additional language parsers:

* `Abaqus <http://en.wikipedia.org/wiki/Abaqus>`_
* `ActionScript <http://en.wikipedia.org/wiki/ActionScript>`_
* `AsciiDoc <http://en.wikipedia.org/wiki/AsciiDoc>`_
* `DocBook <http://en.wikipedia.org/wiki/DocBook>`_
* `Haskell <http://en.wikipedia.org/wiki/Haskell_%28programming_language%29>`_
* `Haxe <http://en.wikipedia.org/wiki/Haxe>`_
* `NSIS <http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System>`_
* `R <http://en.wikipedia.org/wiki/R_%28programming_language%29>`_
* `reStructuredText (reST) <http://en.wikipedia.org/wiki/ReStructuredText>`_
* `txt2tags <http://en.wikipedia.org/wiki/Txt2tags>`_ 


These changes have been merged:

* Fix regex callback match count - https://github.com/fishman/ctags/pull/104 


`VIM-Japan <https://github.com/vim-jp/ctags/>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Anjuta is a Gnome IDE. They did not fork Exuberant ctags, but they did
natively include it in Anjuta. They have made several additions to
thier version of it including fairly extensive Vala langauge support.

tagbar
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Wiki

	https://github.com/majutsushi/tagbar/wiki

	This is a gold mine of xcmd and optlib.

	
External command(xcmd)
----------------------------------------------------------------------

Near feature fishman-ctags can invoke external command as a
specialized parser though some glue code or script may be
needed. Sometimes we may have to hack the external command to adjust
the interface between the command and fishman-ctags.

So let's track external commands maintained out fishman-ctags. If we
prepare glue code or script, mark it with <>, and if not, mark it with
().

<`CoffeeTags <https://github.com/lukaszkorecki/CoffeeTags>`_>
	
	This is the primary target during developing xcmd
	feature. CoffeeTags side hacking is done.

(`perl-tags <https://github.com/dtikhonov/perl-tags>`_)

