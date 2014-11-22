xcmd protocol and writing a driver
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

About the way to utilize external parser commands, see 
"External parser command" in "Changes in fishman-ctags".

Here I explain the way to write a xcmd driver.

Interaction between fishman-ctags and xcmd is explained in "xcmd
protocol". Generally available tags generator like CoffeeTags doesn't
conforms the protocol. Your driver must fill the gap.

Call convention
----------------------------------------------------------------------

ctags invokes ``COMMAND`` specified with ``--<LANG>-xcmd=COMMAND`` 
twice. Once when ctags starts and registers ``COMMAND`` for ``LANG``
to its internal database. Once again when ctags requests ``COMMAND``
to parse an input file and for generating tags information.

At the first time ctags calls ``COMMAND`` with following command line::

	$ COMMAND --list-kinds=LANG

The exit status and standard output are captured by ctags to know 
following two tings.

#. Whether ``COMMAND`` can be available or not.
#. Lists  the  tag  kinds  recognized by ``COMMAND``.

Availability is detected by the exit status of 
``COMMAND`` process; 0 means available.
If the status is other than 0, the ``LANG`` parser is treated
as ``disabled`` with warning messages. 77 is a special
number; the ``LANG`` parser is treated as disabled without
warning messages.



Standard output contributes to know the lists.
ctags expects following format when parsing the output::

  ([^ \t])
  ([^ \t])[ \t]+
  ([^ \t])[ \t]+([^ \t]+)
  ([^ \t])[ \t]+([^ \t]+)[ \t]+
  ([^ \t])[ \t]+([^ \t]+)[ \t]+(.+)

The output lines matched above pattern are recognized as follows::

``\1``

	kind letter

``\2``

	kind name (default ``xcmd``)

``\3``

	kind description (default: ``xcmd`` or kind name if it is given)

Here is the example command line and output of ``coffeetags`` driver::

	$ libexec/drivers/coffeetags --list-kinds=coffee
	f  function
	c  class
	o  object
	v  var
	p  proto
	b  block	

In this case ``\3``, kind description, is empty.

Here after ctags calls ``COMMAND`` with one argument, 
the name of input file::

	$ COMMAND input-file

ctags expects ``COMMAND`` prints the result to standard output.
ctags reads them via a pipe connected to the process of ``COMMAND``.

Note for tags format
----------------------------------------------------------------------

Read `FORMAT <http://ctags.sourceforge.net/FORMAT>`_ about the
expected format of xcmd output. Format 2 is expected. Sort is
done in ctags side if necessary.

Tag lines in the output are merged to the final tags file with
filtering; some fields in the tag lines may be dropped if user
specifies ``--field=-`` option.

In addition to real tag informations, Pseudo-tag lines started
from ``!_TAG_`` are expected.

Following example is taken from ``CoffeeTags``::

	$ libexec/drivers/coffeetags /dev/null
	!_TAG_FILE_FORMAT	2	/extended format/
	!_TAG_FILE_SORTED	0	/0=unsorted, 1=sorted, 2=foldcase/
	!_TAG_PROGRAM_AUTHOR	Łukasz Korecki	/lukasz@coffeesounds.com/
	!_TAG_PROGRAM_NAME	CoffeeTags	//
	!_TAG_PROGRAM_URL	https://github.com/lukaszkorecki/CoffeeTags	/GitHub repository/
	!_TAG_PROGRAM_VERSION	0.5.0	//

ctags merges the Psuedo-tag lines with ``!LANG`` suffix::

	$ ./ctags   --language-force=coffee foo.coffee; cat tags | grep '^!'
	!_TAG_FILE_FORMAT	2	/extended format; --format=1 will not append ;" to lines/
	!_TAG_FILE_SORTED	1	/0=unsorted, 1=sorted, 2=foldcase/
	!_TAG_PROGRAM_AUTHOR	Darren Hiebert	/dhiebert@users.sourceforge.net/
	!_TAG_PROGRAM_AUTHOR!coffee	Łukasz Korecki	/lukasz@coffeesounds.com/
	!_TAG_PROGRAM_NAME	Fishman Ctags	//
	!_TAG_PROGRAM_NAME!coffee	CoffeeTags	//
	!_TAG_PROGRAM_URL	https://github.com/fishman/ctags	/official site/
	!_TAG_PROGRAM_URL!coffee	https://github.com/lukaszkorecki/CoffeeTags	/GitHub repository/
	!_TAG_PROGRAM_VERSION	Development	//
	!_TAG_PROGRAM_VERSION!coffee	0.5.0	//

Integration to the source tree
----------------------------------------------------------------------
Put your xcmd driver under ``libexec/drivers``. This must be an executable;
don't forget dong ``chmod a+x``.

Currently an executable file is written as a sh script; I assumed a
driver may do a few very small things. sh may have enough functions
this purpose and have enough portability. If you need some thing
compiled language like C for writing a driver, we need to add targets
for building and installing the driver to Makefile.in. Remember sh 
doesn't mean bash.

Here is an example taken from ``libexec/drivers/coffeetags``::

	#!/bin/sh
	<<... copyright notices are snipped ...>>
	#
	#
	# This is a xcmd driver for CoffeeTags.
	# CoffeeTags is developed at https://github.com/lukaszkorecki/CoffeeTags .
	#
	#
	case "$1" in
	--list-kinds*)
		coffeetags --list-kinds
		exit $?
		;;
	-*)
		echo "unknown option: $1" 1>&2
		exit 1
		;;
	*)
		coffeetags --include-vars "$1"
		exit $?
		;;
	esac

An optlib file is also needed to let ctags know the driver.
Here is an example taken from ``data/optlib/coffee.ctags``::

	#
	<<... copyright notices are snipped ...>>
	#
	--langdef=coffee
	--coffee-map=+.coffee
	--coffee-xcmd=coffeetags

Finnaly you have to add these new two files to ``Makefile.in``.
Add the name of driver file to ``DRIVERS`` variable like::

	DRIVERS = coffeetags

Then add the name of optlib file to ``PRELOAD_OPTLIB`` or
``OPTLIB`` like::

	PRELOAD_OPTLIB =    \
		\
		coffee.ctags \
		...

If you add the optlib file to ``OPTLIB``, it will not loaded
automatically when ctags starts.


NOTE for writing a test case for xcmd
----------------------------------------------------------------------

You may want to test the output merged from a xcmd.
The test for xcmd should be conducted only if the xcmd
is available.

Consider a system where coffeetags command is not installed,
running test cases for coffeetags are meaningless. This
means a stage for checking the availability of xcmd is
needed before running a test case.

*Units/TEST/languages* is for the purpose. See "How to write a test case"
in "Using *Units*".

