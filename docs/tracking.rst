:orphan:

Tracking other projects
----------------------------------------------------------------------

This is working note for tracking activities other projects,
especially activity at Exuberant Ctags.

I(Masatake YAMATO) consider tracking activities as the first class
fruits of this project.


Exuberant Ctags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

subversion
......................................................................

* status

  Revisions up to <r815> are merged except:

	NOTHING HERE NOW

  (Mon Sep 22 12:41:32 2014 by yamato)

* howto

  ::

      <svn>
      => <git: local Universal Ctags repo>
      => <git: local Universal Ctags repo>


  1. prepare your own Universal Ctags repo: a local git repo cloned from github.
     You may know how to do it :)

     ::

	$ git clone https://github.com/universal-ctags/ctags.git

  2. prepare Exuberant Ctags SVN repo: a local git repo clone from Exuberant Ctags svn tree.

    The original clone is already part of Exuberant Ctags tree.

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

     4.1. Make a branch at local Universal Ctags repo and switch to it.

     4.2. Do cherry-pick like::

	 	$ git cherry-pick -s -x c81a8ce

     You can find commit id on the another terminal
     <git: local Universal Ctags repo>::

	 	$ git log

     or ::

	 	$ git log --oneline

     If conflicts are occurred in cherry-picking, you can
     abort/reset cherry-picking with::

	 $ git reset --hard

     <git: local Universal Ctags repo>
      at the branch for picking.

bugs
......................................................................
   <367>  C++11 override makes a C++ member function declaration ignored

	 * fixed in::

	        d4fcbdd
		#413
		#405

   <366>  --options=.ctags doesn't work under Windows

	 * fixed in::

	        15cedc6c94e95110cc319b5cdad7807caf3db1f4

   <365>  Selecting Python kinds is broken

	* fixed in::

	         4a95e4a55f67230fc4eee91ffb31c18c422df6d3

	* discussed at #324.

   <364>  Ruby method on self is missing the trailing ? in the generated tag name

	 * fixed in::

	        d9ba5df9f4d54ddaa511bd5440a1a3decaa2dc28

   <363> Invalid C input file causes invalid read / heap overflow

	* it is not reproduced.

	* the test case is imported as parser-c.r/c-heapoverflow-sh-bug-363.d::

   		$ make units UNITS=c-heapoverflow-sh-bug-363 VG=1

   <361> Invalid C input file causes invalid read / heap overflow

	* it is not reproduced.

   <360> Fails to parse annotation's fields with default value

	* fixed in::

		682a7f3b180c27c1196f8a1ae662d6e8ad142939

   <358>  Vim parser: Segmentation fault when reading empty vim file

	 * directly contributed by the original author of bug report and patch::

	   	e0f854f0100e7a3cb8b959a23d6036e43f6b6c85

	 * it is fixed in sf, too::

	   	5d774f6022a1af71fa5866994699aafce0253085

   <356> [python] mistakes module level attribute for class level attribute in module level if

	 * fixed in::

	        ab91e6e1ae84b80870a1e8712fc7f3133e4b5542

   <355> Error when parsing empty file (OCaml)

	 * fixed in::

	   	02ec2066b5be6b129eba49685bd0b17fef4acfa

   <341> Lua: "function f ()" whitespace

	 * fixed in::

	   	8590bbef5fcf70f6747d509808c29bf84342cd0d

   <341> Introducing ctags.conf.d

	 * merged the improved version::

	   	216880c5287e0421d9c49898d983144db61c83aa

   <271> regex callback is broken; <320> [PATCH] fix regex callback match count

	 * merged patch (with updated bug number)::

		a12b3a24b62d6535a968e076675f68bac9ad32ba

   <177> Lua: "function" results in function tag (includes patch)

	 * fixed in::

	   	5606f3f711afeac74587a249650a5f7b416f19be

`patches <https://sourceforge.net/p/ctags/patches/%d>`_
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tracking the tickets in patch tracker is quite fruitful.
Patches are always there. So it is easy to evaluate the value:)

   [(<]TICKET#[>)] TITLE

	* STATUS

	  + MORE STATUS

   <TICKET#>

   	means the ticket is closed from the view of Exuberant Ctags tree
   	developers.  We don't have to take time for this ticket.

   (TICKET#)

   	means the ticket is still opened from the view of Exuberant Ctags
	tree developers.  We don't have to take time for this ticket.

----

   <85> Add --encoding option to make utf-8 encoded tags file

	* contributed by the original author::

	      b3f670c7c4a3c3570b8d2d82756735586aafc0cb

   <84> C++11 new using semantics

	* solved by another implementation::

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

   <74> protobuf parser

	* Merged after getting approval from the original author

   <67> Objective C language parser

	* This is the implementation we have in Universal Ctags tree.

   <65> absoluteFilename uses strcpy on overlapping strings

	* Fixed in Universal Ctags tree, however the ticket is still open::

   		d2bdf505abb7569deae2b50305ea1edce6208557

   <64> Fix strcpy() misuse

	* Fixed in Universal Ctags tree, however the ticket is still open::

		d2bdf505abb7569deae2b50305ea1edce6208557

   <55> TTCN-3 support

	* contributed by the original author

   <51> Ada support

	* Ada support is now available in Universal Ctags tree::

		4b6b4a72f3d2d4ef969d7c650de1829d79f0ea7c

   <38> Ada support

	* Ada support is now available in Universal Ctags tree::

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
	https://en.wikipedia.org/wiki/Resource_%28Windows%29

	::

	 	95b4806ba6c006e4b7e72a006700e33c720ab9e7


    ([Ctags-devel] Skip repeat PATH_SEPARATORs in relativeFilename())
    From: Seth Dickson <whefxlr@gm...> - 2013-12-24 04:51:01

	Looks interesting.


Fedora
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some patches are maintained in ctags package of Fedora.
Inventory of patches are
http://pkgs.fedoraproject.org/cgit/ctags.git/tree/ctags.spec

<ctags-5.7-destdir.patch>

	This patch was merged in Universal Ctags git tree::

		d4b5972427a46cbdcbfb050a944cf62b300676be

<ctags-5.7-segment-fault.patch>

	This patch was merged in Universal Ctags git tree::

		8cc2b482f6c7257c5151893a6d02b8c79851fedd

(ctags-5.8-cssparse.patch)

	Not in Universal Ctags tree.

	The reproducer is attached to the following page:
	https://bugzilla.redhat.com/show_bug.cgi?id=852101

	However, Universal Ctags doesn't reproduce with it.

	I, Masatake YAMATO, read the patch.  However, I don't
	understand the patch.

<ctags-5.8-css.patch>

	This patch was merged in Universal Ctags git tree::

		80c1522a36df3ba52b8b7cd7f5c79d5c30437a63

<ctags-5.8-memmove.patch>

	This patch was merged in Exuberant Ctags svn tree.
	As the result this patch is in Universal Ctags tree::

		d2bdf505abb7569deae2b50305ea1edce6208557

<ctags-5.8-ocaml-crash.patch>

	This patch was merged in Exuberant Ctags svn tree.
	As the result this patch is in Universal Ctags tree::

		ddb29762b37d60a875252dcc401de0b7479527b1

<ctags-5.8-format-security.patch>

	This patch was merged in Exuberant Ctags svn tree.
	As the result this patch is in Universal Ctags tree::

		2f7a78ce21e4156ec3e63c821827cf1d5680ace8

Debian
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some patches are maintained in ctags package of Debian.
Inventory of patches are
http://anonscm.debian.org/cgit/users/cjwatson/exuberant-ctags.git/tree/debian/patches/series

<python-disable-imports.patch>

	Universal Ctags tags Y in `import X as Y` and Z in `from X import Y as Z`
	as definition tags. They are turned on by default.
	The others are tagged as reference tags. reference tags are recorded only
	when "r" extra tags are enabled. e.g. `--extras=+r`.

<vim-command-loop.patch>

	This patch was merged as an alternative for
	7fb36a2f4690374526e9e7ef4f1e24800b6914ec

	Discussed on https://github.com/fishman/ctags/issues/74

	::

	   	e59325a576e38bc63b91abb05a5a22d2cef25ab7
