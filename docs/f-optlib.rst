Contributing an optlib
============================================================

You are welcome.

fishman-ctags provides a facility for "Option library".
Read "Option library" about the concept and usage first.

Here I will explain how to merge your .ctags into fishman-ctags as
part of option library. Here I assume you consider contributing
an option library in which a regex based language parser is defined.
See `How to Add Support for a New Language to Exuberant Ctags (EXTENDING)`_
about the way to how to write a regex based language parser. In this
section I explains the next step.

.. _`How to Add Support for a New Language to Exuberant Ctags (EXTENDING)`: http://ctags.sourceforge.net/EXTENDING.html

I use Swine as the name of programming language which your parser
deals. Assume source files written in Swine language have a suffix
*.swn*. The file name of option library is *swine.ctags*.

Changes in options
---------------------------------------------------------------------
fishman-ctags prepares aliases for options. 

========================= ====================
Exuberant                 Fishman aliases
========================= ====================
``--langmap-swine:.swn``  ``--swine-map=.swn``
``--regex-swine=...``     ``--swine-regex=...``
========================= ====================

These are jst aliases. So the original options can be used.
The reason I introduced these aliases, I want option syntax
more language/parser centric. I'm not sure this idea is good
or not.

Copyright notice, contact mail address and license term
---------------------------------------------------------------------

Put these information at the header of *swine.ctags*.

An example taken from *data/optlib/ctags.ctags* ::

    #
    #
    #  Copyright (c) 2014, Red Hat, Inc.
    #  Copyright (c) 2014, Masatake YAMATO
    #
    #  Author: Masatake YAMATO <yamato@redhat.com>
    #
    # This program is free software; you can redistribute it and/or
    # modify it under the terms of the GNU General Public License
    # as published by the Free Software Foundation; either version 2
    # of the License, or (at your option) any later version.
    #
    # This program is distributed in the hope that it will be useful,
    # but WITHOUT ANY WARRANTY; without even the implied warranty of
    # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    # GNU General Public License for more details.
    #
    # You should have received a copy of the GNU General Public License
    # along with this program; if not, write to the Free Software
    # Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
    # USA.
    #
    #
    ...

"GPL version 2 or later version" is needed here.  Option file is not
linked to ctags command. However, I have a plan to write a translator
which generates *.c* file from a given option file. As the result the
*.c* file is built into *ctags* command. In such case "GPL version 2
or later version" may be required.

*Units* test cases
---------------------------------------------------------------------

We, fishman-ctags developers don't have enough time to learn all
languages supported by ctags. In other word, we cannot review the
code. Only test cases help us to know whether a contributed option
library works well or not. We may reject any contribution without
a test case.

Read "Using *Units*" about how to write *Units* test
cases.  Don't write one big test case. Some smaller cases are helpful
to know about the intent of the contributor.

* *Units/sh-alias.d*
* *Units/sh-comments.d*
* *Units/sh-quotes.d*
* *Units/sh-statements.d*

are good example of small test cases.n
Big test cases are good if smaller test cases exist.

See also *mib-simple.d* especially *mib-simple.d/args.ctags*.
Your test cases need ctags having already loaded your option
library, swine.ctags. You must specify loading it in the 
test case own *args.ctags*.

Assume your test name is *swine-simile.d*. Put ``--option=swine`` in
*Units/swine-simile.d/args.ctags*.


Corpus data
---------------------------------------------------------------------

Corpus data is helpful for ctags to choose a proper language parser.
READ "Automatic parser selection based on corpora" subsection how 
Corpus data are used in ctags.

What you need is not so difficult. Prepare lexically typical source
file written in Swine language. ctags learns the lexical
characteristic of Swine from the source file.

Assume you prepare *swine.swn* as corpus data for ``Swine`` 
language parser.

1. put it to data/corpora/swine.swn, and
2. add ``--swine-corpus=.swn:swine.swn`` to swine.ctags.


Makefile.in
---------------------------------------------------------------------
Add your option library file and associative corpus file to 
*Makefile.in*.

1. add your optlib file, *swine.ctags* to ``PRELOAD_OPTLIB`` variable of *Makefile.in*, and
2. add your corpus file, *swine.swn* to ``CORPORA`` variable of *Makefile.in*.

If you don't want your optlib loaded automatically when ctags starting up,
put your optlib file to ``OPTLIB`` of *Makefile.in* instead of 
``PRELOAD_OPTLIB``.

Verification
---------------------------------------------------------------------

Let's verify all your work here.

1. Run the tests and check whether your test case is passed or failed::

	$ make -f testing.mak test.units

2. Verify your files are installed as expected::

	$ mkdir /tmp/tmp
	$ ./configure --prefix=/tmp/tmp
	$ make
	$ make install
	$ /tmp/tmp/ctags -o - --option=swine something_input.swn


Pull-request
---------------------------------------------------------------------

Remember your *.ctags* is treasure and can be shared as a first class
software component in fishman-ctags.  Again, pull-requests are welcome.
