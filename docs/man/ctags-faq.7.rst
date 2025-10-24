.. _ctags-faq(7):

==============================================================
ctags-faq
==============================================================

Universal Ctags FAQ

:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

This is the Universal Ctags FAQ (Frequently-Asked Questions).
It is based on `Exuberant Ctags FAQ <http://ctags.sourceforge.net/faq.html>`_

.. contents::

DESCRIPTION
-----------

.. TODO: https://github.com/universal-ctags/ctags/issues/2312
	#1421: feature: clean up stale tags when appending (`-a`)
	#2356: can't pre-process the macro but it works with Exuberant Ctags 5.8
	#2540: C/C++：conditional compilation like #ifdef will cause parse errror

What is the difference between Universal Ctags and Exuberant Ctags?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Universal Ctags is an unofficial fork of Exuberant Ctags.
The differences are summarized in :ref:`ctags-incompatibilities(7) <ctags-incompatibilities(7)>` man page.

The most notable one is that Universal Ctags doesn't read ``~/.ctags`` file.
Instead, it reads ``*.ctags`` under ``~/.ctags.d`` directory.

How can I avoid having to specify my favorite option every time?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Either by setting the environment variable ``CTAGS`` to your custom
options, or putting them into a ``~/.ctags.d/anyname.ctags`` file in your home
directory.

What are these strange bits of text beginning with ``;"`` which follow many of the lines in the tag file?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

These are *extension flags*. They are added in order to provide extra
information about the tag that may be utilized by the editor in order to
more intelligently handle tags. They are appended to the EX command part of
the tag line in a manner that provides backwards compatibility with existing
implementations of the Vi editor. The semicolon is an EX command separator
and the double quote begins an EX comment. Thus, the extension flags appear
as an EX comment and should be ignored by the editor when it processes the
EX command.

Some non-vi editors, however, implement only the bare minimum of EX commands
in order to process the search command or line number in the third field of
the tag file. If you encounter this problem, use the option ``--format=1`` to
generate a tag file without these extensions (remember that you can set the
CTAGS environment variable to any default arguments you wish to supply). Then
ask the supplier of your editor to implement handling of this feature of EX
commands.

Why can't I jump to ``class::member``?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Because, by default, ctags only generates tags for the separate identifiers
found in the source files. If you specify the ``--extra=+q`` option, then
ctags will also generate a second, class-qualified tag for each class member
(data and function/method) in the form ``class::member`` for C++, and in the form
``class.method`` for Eiffel and Java.

Why do I end up on the wrong line when I jump to a tag?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

By default, ctags encodes the line number in the file where macro (``#define``)
tags are found. This was done to remain compatible with the original UNIX
version of ctags. If you change the file containing the tag without
rebuilding the tag file, the location of tag in the tag file may no longer
match the current location.

In order to avoid this problem, you can specify the option ``--excmd=p``,
which causes ctags to use a search pattern to locate macro tags. I have
never uncovered the reason why the original UNIX ctags used line numbers
exclusively for macro tags, but have so far resisted changing the default
behavior of Exuberant (and Universal) Ctags to behave differently.

How do I jump to the tag I want instead of the wrong one by the same name?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A tag file is simple a list of tag names and where to find them. If there
are duplicate entries, you often end up going to the wrong one because the
tag file is sorted and your editor locates the first one in the tag file.

Standard Vi provides no facilities to alter this behavior. However, Vim
has some nice features to minimize this problem, primarily by examining all
matches and choosing the best one under the circumstances. Vim also provides
commands which allow for selection of the desired matching tag.

How can I locate all references to a specific function or variable?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are several packages already available which provide this capability.
Namely, these are: GLOBAL source code tag system, GNU id-utils, cscope,
and cflow. As of this writing, they can be found in the following locations:

- GLOBAL:    http://www.gnu.org/software/global
- id-utils:  http://www.gnu.org/software/idutils/idutils.html
- cscope:    http://cscope.sourceforge.net
- cflow:     ftp://www.ibiblio.org/pub/Linux/devel/lang/c

Why does appending tags to a tag file tag so long?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes, in an attempt to build a global tag file for all source files in
a large source tree of many directories, someone will make an attempt to run
ctags in append (``-a``) mode on every directory in the hierarchy. Each time
ctags is invoked, its default behavior is to sort the tag file once the tags
for that execution have been added. As the cumulative tag file grows, the sort
time increases arithmetically.

The best way to avoid this problem (and the most efficient) is to make
use of the ``--recurse`` (or ``-R``) option of ctags by executing the following
command in the root of the directory hierarchy (thus running ctags only once):

	.. code-block:: sh

		ctags -R

If you really insist on running ctags separately on each directory, you can
avoid the sort pass each time by specifying the option ``--sort=no``. Once the
tag file is completely built, use the sort command to manually sort the
final tag file, or let the final invocation of ctags sort the file.

How should I set up tag files for a multi-level directory hierarchy?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are a few ways of approaching this:

1.  A local tag file in each directory containing only the tags for source
    files in that directory.

2.  One single big, global tag file present in the root directory of your
    hierarchy, containing all tags present in all source files in the
    hierarchy.

3.  A local tag file in each directory containing only the tags for source
    files in that directory, in addition to one single global tag file
    present in the root directory of your hierarchy, containing all
    non-static tags present in all source files in the hierarchy.

4.  A local tag file in each directory of the hierarchy, each one
    containing all tags present in source files in that directory and all
    non-static tags in every directory below it (note that this implies
    also having one big tag file in the root directory of the hierarchy).

Each of these approaches has its own set of advantages and disadvantages,
depending upon your particular conditions. Which approach is deemed best
depends upon the following factors:

A.  The ability of your editor to use multiple tag files.

    If your editor cannot make use of multiple tag files (original vi
    implementations could not), then one large tag file is the only way to
    go if you ever desire to jump to tags located in other directories. If
    you never need to jump to tags in another directory (i.e. the source
    in each directory is entirely self-contained), then a local tag file
    in each directory will fit your needs.

B.  The time is takes for your editor to look up a tag in the tag file.

    The significance of this factor depends upon the size of your source
    tree and on whether the source files are located on a local or remote
    file system. For source and tag files located on a local file system,
    looking up a tag is not as big a hit as one might first imagine, since
    vi implementations typically perform a binary search on a sorted tag
    file. This may or may not be true for the editor you use. For files
    located on a remote file system, reading a large file is an expensive
    operation.

C.  Whether or not you expect the source code to change and the time it
    takes to rebuild a tag file to account for changes to the source code.

    While Universal Ctags is particularly fast in scanning source code
    (around 1-2 MB/sec), a large project may still result in objectionable
    delays if one wishes to keep their tag file(s) up to date on a
    frequent basis, or if the files are located on a remote file system.

D.  The presence of duplicate tags in the source code and the ability to
    handle them.

    The impact of this factor is influenced by the following three issues:

    1.  How common are duplicate tags in your project?

    2.  Does your editor provide any facilities for dealing with duplicate
        tags?

        While standard vi does not, many modern vi implementations, such
        as Vim have good facilities for selecting the desired match from
        the list of duplicates. If your editor does not support duplicate
        tags, then it will typically send you to only one of them, whether
        or not that is the one you wanted (and not even notifying you that
        there are other potential matches).

    3.  What is the significance of duplicate tags?

        For example, if you have two tags of the same name from entirely
        isolated software components, jumping first to the match found
        in component B while working in component A may be entirely
        misleading, distracting or inconvenient (to keep having to choose
        which one if your editor provides you with a list of matches).
        However, if you have two tags of the same name for parallel builds
        (say two initialization routines for different hosts), you may
        always want to specify which one you want.

Of the approaches listed above, I tend to favor Approach 3. My editor of
choice is Vim, which provides a rich set of features for handling multiple
tag files, which partly influences my choice. If you are working with
source files on a remote file system, then I would recommend either
Approach 3 or Approach 4, depending upon the hit when reading the global
tag file.

The advantages of Approach 3 are many (assuming that your editor has
the ability to support both multiple tag files and duplicate tags). All
lookups of tag located in the current directory are fast and the local
tag file can be quickly and easily regenerated in one second or less
(I have even mapped a keystroke to do this easily). A lookup of a
(necessarily non-static) tag found in another directory fails a lookup in
the local tag file, but is found in the global tag file, which satisfies
all cross-directory lookups. The global tag file can be automatically
regenerated periodically with a cron job (and perhaps the local tag files
also).

Now I give an example of how you would implement Approach 3. Means of
implementing the other approaches can be performed in a similar manner.

Here is a visual representation of an example directory hierarchy:

::

	project
	`-----misccomp
	|       `...
	`-----sysint
	        `-----client
	        |       `-----hdrs
	        |       `-----lib
	        |       `-----src
	        |       `-----test
	        `-----common
	        |       `-----hdrs
	        |       `-----lib
	        |       `-----src
	        |       `-----test
	        `-----server
	                `-----hdrs
	                `-----lib
	                `-----src
	                `-----test

Here is a recommended solution (conceptually) to build the tag files:

1.  Within each of the leaf nodes (i.e. ``hdrs``, ``lib``, ``src``, ``test``) build a tag
    file using "``ctags *.[ch]``". This can be easily be done for the whole
    hierarchy by making a shell script, call it ``dirtags``, containing the
    following lines:

	.. code-block:: sh

		#!/bin/sh
		cd $1
		ctags *

    Now execute the following command:

	.. code-block:: sh

		find * -type d -exec dirtags {} \;

    These tag files are trivial (and extremely quick) to rebuild while
    making changes within a directory. The following Vim key mapping is
    quite useful to rebuild the tag file in the directory of the current
    source file:

	.. code-block:: text

		:nmap ,t :!(cd %:p:h;ctags *.[ch])&<CR><CR>

2.  Build the global tag file:

	.. code-block:: sh

		cd ~/project
		ctags --file-scope=no -R

    thus constructing a tag file containing only non-static tags for all
    source files in all descendent directories.

3.  Configure your editor to read the local tag file first, then consult
    the global tag file when not found in the local tag file. In Vim,
    this is done as follows:

	.. code-block:: text

		:set tags=./tags,tags,~/project/tags

If you wish to implement Approach 4, you would need to replace the
``dirtags`` script of step 1 with the following:

	.. code-block:: sh

		#!/bin/sh
		cd $1
		ctags *
		# Now append the non-static tags from descendent directories
		find * -type d -prune -print | ctags -aR --file-scope=no -L-

And replace the configuration of step 3 with this:

	.. code-block:: text

		:set tags=./tags;$HOME,tags

As a caveat, it should be noted that step 2 builds a global tag file whose
file names will be relative to the directory in which the global tag file
is being built. This takes advantage of the Vim ``tagrelative`` option,
which causes the path to be interpreted a relative to the location of the
tag file instead of the current directory. For standard vi, which always
interprets the paths as relative to the current directory, we need to
build the global tag file with absolute path names. This can be
accomplished by replacing step 2 with the following:

	.. code-block:: sh

		cd ~/project
		ctags --file-scope=no -R `pwd`

Does Universal Ctags support Unicode file names?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. MEMO: from https://github.com/universal-ctags/ctags/issues/1837

Yes, Unicode file names are supported on unix-like platforms (Linux, macOS,
Cygwin, etc.).

However, on MS Windows, you need to use Windows 10 version 1903 or later to use
Unicode file names. (This is an experimental feature, though.) On older versions
on Windows, Universal Ctags only support file names represented in the current
code page. If you still want to use Unicode file names on them, use Cygwin or
MSYS2 version of Universal Ctags as a workaround.

Why does zsh cause "zsh: no matches found" error?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. MEMO: from https://github.com/universal-ctags/ctags/issues/2842

zsh causes error on the following cases;

	.. code-block:: sh

		ctags --extra=+* ...
		ctags --exclude=foo/* ...

This is the 2nd most significant incompatibility *feature* of zsh.

Cited from "Z-Shell Frequently-Asked Questions", "`2.1: Differences from sh and
ksh <http://zsh.sourceforge.net/FAQ/zshfaq02.html>`_";

	... The next most classic difference is that unmatched glob patterns cause
	the command to abort; set ``NO_NOMATCH`` for those.

You may add "``setopt nonomatch``" on your ``~/.zshrc``. Or you can escape glob
patterns with backslash;

	.. code-block:: sh

		ctags --extra=+\* ...
		ctags --exclude=foo/\* ...

Or quote them;

	.. code-block:: sh

		ctags '--extra=+*' ...
		ctags '--exclude=foo/*' ...

SEE ALSO
--------

The official Universal Ctags web site at:

https://ctags.io/

:ref:`ctags(1) <ctags(1)>`, :ref:`tags(5) <tags(5)>`

AUTHOR
------

This FAQ is based on `Exuberant Ctags FAQ <http://ctags.sourceforge.net/faq.html>`_ by
Darren Hiebert and vberthoux@users.sourceforge.net

Universal Ctags project: https://ctags.io/
