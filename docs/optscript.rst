.. _optscript:

Optscript, a programming language for extending optlib parsers
--------------------------------------------------------------

.. contents:: `Table of contents`
	:depth: 3
	:local:

Preparation for learning
~~~~~~~~~~~~~~~~~~~~~~~~
**Optscript** is an implementation of PostScript(tm) alike stack
oriented general purpose programming language.  Developers of optlib
parsers can utilize the language for extending their parsers.

You may not be familiar with a stack oriented programming language.
Though there are some differences, the syntax and core non-graphical
operators of Optscript and PostScript are the same. You can get the
basic knowledge for using Optscript from the materials for learning
PostScript.

"PostScript Language Tutorial & Cookbook" published by Adobe Systems
Inc. The book is known as "blue book". This is the best place to
start.  PostScript is a language for controlling printers. So it has
many graphical operators. Optscript is for tagging, and doesn't have
such graphical operators. So you can skip the sections about graphics
(but you may want to read them because the book is written well).

Ghostscript (``gs`` or ``gsnd``) is an interpreter for the PostScript
language and PDF files. Unlike Optscript, it implements the full-set of
PostScript features including graphical operators. It is available
under either the GNU GPL Affero license. You can Ghostscript while
reading the blue book. Do web searching to know about Ghostscript.

``optscript`` is an command that source files are included in
Universal Ctags source tree. You can use it as the replacement of
``gs``. However, I recommend you to have ``gs`` at hand because
``optscript`` may have bugs. ``gs`` is much mature than ``optscript``.
Having two interpreters helps you to know correct behavior.

Though ``gs`` has much higher qualities than ``optscript``, eventually
you may have to build the ``optscript`` command to learn Optscript
specific operators. You can built the command with "``make
optscript``".

* red book

  TBW

Syntax extension
~~~~~~~~~~~~~~~~~~~~~~~~

``?`` is a prefix for representing a character literal.

For an example, ``?x`` represents 120. This is a short cut for ``(x) 0
get``.

Some characters has special notation using ``\``.

``?\t``

	tab

``?\n``

	newline

``?\_``

	space


The ``optscript`` command
~~~~~~~~~~~~~~~~~~~~~~~~~

You can run optscript with no argument:

.. code-block:: console

	$ ./optsript
	OPT>

``OPT>`` is the prompt of the interpreter.
You can stop it with ``quit`` operator:

.. code-block:: console

	$ ./optsript
	OPT> quit
	$


Let's see some example sessions.  To help you understand the session
easily, Python sessions doing the same as Optscript are also written.

* hello world

  Optscript:

  .. code-block:: console

	OPT> (hello, world) =
	hello, world

  Python:

  .. code-block:: console

	>>> print ('hello, world')
	hello, world

* Adding

  Optscript:

  .. code-block:: console

	OPT> 2 3 add =
	5

  Python:

  .. code-block:: console

	>>> print (2 + 3)
	5

* Variables

  Optscript:

  .. code-block:: console

	OPT> /x 2 def
	OPT> /y 3 def
	OPT> x y add =
	5

  Python:

  .. code-block:: console

	>>> x = 2
	>>> y = 3
	>>> print (x + y)
	5

* Procedures

  Optscript:

  .. code-block:: console

	OPT> /add5_and_print { 5 add = } def
	OPT> 4 add5_and_print
	9

  Python:

  .. code-block:: console

	>>> def add5_and_print(x):
	...    print(x + 5);
	>>> add5_and_print(4)
	9

* string manipulation

  TBW

* array manipulation

  TBW

* dict manipulation

  TBW

* control flow

  TBW

* operators for printing

  TBW

* reading script from file

  TBW

Optscript in ctags
~~~~~~~~~~~~~~~~~~

Related options
...............

.. code-block:: ctags

	--_prelude-<LANG>={{
		OPTSCRIPT CODE FRAGMENTS
	}}

	--_sequel-<LANG>={{
		OPTSCRIPT CODE FRAGMENTS
	}}

	--regex-<LANG>=<PATTERN>/<NAME>/[<KIND>/]LONGFLAGS...{{
		OPTSCRIPT CODE FRAGMENTS
	}}

	--regex-<LANG>=<PATTERN>//LONGFLAGS...{{
		OPTSCRIPT CODE FRAGMENTS
	}}

	--mline-regex-<LANG>=<PATTERN>/<NAME>/[<KIND>/]LONGFLAGS...{{
		OPTSCRIPT CODE FRAGMENTS
	}}

	--mline-regex-<LANG>=<PATTERN>//LONGFLAGS...{{
		OPTSCRIPT CODE FRAGMENTS
	}}

	--_mtable-regex-<LANG>=<TABLE>/<PATTERN>/<NAME>/[<KIND>/]LONGFLAGS...{{
		OPTSCRIPT CODE FRAGMENTS
	}}

	--_mtable-regex-<LANG>=<TABLE>/<PATTERN>//LONGFLAGS...{{
		OPTSCRIPT CODE FRAGMENTS
	}}

	--_list-operators

	--list-fields

	--_paramdef-<LANG>=<NAME>,<DESCRIPTION>

You can run optscript code fragments when pattern specified with
options matches successfully. The options are ``--regex-<LANG>``,
``--mline-regex-<LANG>``, and ``--_mtable-regex-<LANG>`` as you
expect. In addition, ``--_prelude-<LANG>`` and ``--_sequel-<LANG>``
options also take code fragments.

TBW: two timings of evaluation

Put code fragments at the end of options with surrounding "``{{``" and
"``}}``". Though it is not impossible, a command line is not suitable
place to put code fragments because the code fragments may be long.
Instead, you should write them to a .ctags file.

.. warning::  An important rule in writing Optscript code in a file is
  the start marker, ``{{`` must be at the end of line, and the end
  marker ``}}`` must be at the beginning of line. If you break the
  rule, the optlib loader of ctags fails to read your file.

``--_prelude-<LANG>`` is for specified code fragments running at the
beginning of parsing a source file. You can use this option for
defining the common code used in the parser.

``--_sequel-<LANG>`` is for for specified code fragments running at the end
of parser a source file. Other than clean-up operations, the typical
use case of this option is for filling ``end:`` field of the tags
ctags found at the end of the input file.

Consider the following input, written in a Python-like pseudocode:
.. code-blcok::

	...
	def fun(args)
		...
	EOF

You can make a tag with ``--regex-PseudoLang=/^def[ \t]+([a-f]+)/\1/d/``.
However, there was no way to fill the ``end:`` field of ``f``.
Here ``--_sequel-<LANG>`` option comes in. ctas runs the code fragment
specified with the option after reaching the end of input.

.. code-block:: ctags
	--langdef=PseudoLang
	--map-PseudoLang=.ppp
	--kinddef-PseudoLang=d,def,definitions
	--regex-PseudoLang=/^def[ \t]+([a-f]+)/\1/d/{{
		% New definition
		% Let's fill end: field of the tags on the stack
		count 0 gt {
			% Push the the line number (currentLine) of the tag (currentTag)
			% just extracted.
			. :line
			% The last tag extracted before extracting currentTag ends
			% at currentLine - 1.  (lastLine)
			1 sub
			% Fill the end: field with the lastLine.
			end:
		} if
		% push the currentTag
		.
		% Unless more `def` is found, we have no chance to fill
		% the end: field of the currentTag. but ...
	}}
	--_sequel-PseudoLang={{
		% this option gives the chance to fill the field.
		count 0 gt {
			. :line end:
		} if
	}}

``.`` in ``--_sequel-<LANG>`` code fragment is a placeholder tag.
ctags makes the placeholder tag when executing the code fragment
implicitly. The placeholder tag is only for providing the line number
of the end of input file. You can get the line number with
``. :line``.

You can also use this option for debug-printing the final state of
parsing the source file.  e.g. ``--_sequel-Foo={{ _traced { pstack }
if }}``.

``--_list-operators`` lists all operators (and built-in procedures)
and exits. In additions to operators defined in  ``optscript``,
``ctags`` provides operators for tagging.

``OP`` column of ``--list-fields`` represents the availability of
operators for accessing the field specified in the line.  ``r``
represents the field has an operator for reading
(``:fieldname``). ``w`` represents the field has an operator for
writing (``fieldname:``).

``--_paramdef-<LANG>=<NAME>,<DESCRIPTION>`` defines a language specific
parameter named  ``<NAME>``. ``--param-<LANG>.<NAME>=<VALUE>`` assigns
a value ``<VALUE>`` to the parameter. You can access the value from
Optscript code with ``_param`` operator. Don't use ``{`` character
in the description. The character is reserved for future extension.

Operators
............................

**.** -> ``-`` **.** ``corkIndex:int``

    Push the cork index for the tag.

**\\n** -> ``-`` **\\n** ``matchedString:string``

	``n`` is an integer (0...9) representing a group in a pattern.
	Push the matched string for the group.

``_matchloc``

    TBW

``:field`` (See the output of ``--_list-operators``)

    Get the value for the specified field from a tag
	and put it.

``field:`` (See the output of ``--_list-operators``)

    Set a value at the stack to the specified field of a tag.

``_tag``

    TBW

``_COMMIT``

    TBW

``_traced``

    TBW

Data types
..........

``MATCHLOC``

    TBW

``index:int``

    TBW

``TAG``

    TBW

Recipes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Parameters
...........................................
With ``--_paramdef-<LANG>=<param>,<description>`` option, you can
define a parser-specific parameter to your optlib parser.

In the following example, we define a parser, ``Foo`` with a
parser-specific parameter ``VAR``. If the parser sees "x" in the
current input stream, a code fragment attached to the pattern "/(x)/"
runs. The code fragment does (1) check whether a value for ``VAR`` is
given, (2) emit a tag with the value given to parameter ``VAR`` as
name and with ``xval`` kind, and (3) remove the cork index from the
operand stack.

foo.ctags:
.. code-block::

	--langdef=Foo
	--map-Foo=.foo
	--kinddef-Foo=v,xval,externally defined values
	--_paramdef-Foo=VAR,desc
	--regex-Foo=/(x)//{{
		/VAR _param { % if VAR is defined ....
			/xval _tag _commit pop
		} if
		% if VAR is not defined, we do nothing.
	}}

input.foo:
.. code-block::

	x

When running the parser, we can give a value ("hereiam" in the following example)
for the parameter ``VAR`` from the command line:

.. code-block:: console

	$ ./ctags --options=args.ctags --param-Foo.VAR=hereiam -o - input.foo
	hereiam	input.foo	/^x$/;"	v

Difference between Optscript and PostScript
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Memory management

* Dynamically extendable data type

  - string

  - array
