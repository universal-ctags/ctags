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

"PostScript Language Tutorial & Cookbook" is a book published by Adobe
Systems Inc. The book, known as "blue book", is the best place to
start.  PostScript is a language for controlling printers. So it has
many graphical operators. Optscript doesn't have such graphical
operators. So you can skip the sections about graphics
(but you may want to read them because the book is written well).

Ghostscript (``gs`` or ``gsnd``) is an interpreter for the PostScript
language and PDF files. Unlike Optscript, it implements the full-set of
PostScript features including graphical operators. It is available
under either the GNU GPL Affero license. You can use Ghostscript while
reading the blue book. Do web searching to know about Ghostscript.

``optscript`` is a command that source files are included in the
source tree of Universal Ctags. You can use it as the replacement of
``gs`` to learn PostScript. However, I recommend you to have ``gs`` at
hand because ``optscript`` may have bugs. ``gs`` is much mature than
``optscript``. Having two interpreters helps you to know expected
behavior of the language.

Though ``gs`` has much higher qualities than ``optscript``, eventually
you may have to build the ``optscript`` command to learn Optscript
specific operators for tagging. You can built the command with "``make
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

* String manipulation

  - Comparison

	Optscript:

	.. code-block:: console

	   OPT> (abc) (efg) eq { (same) = } { (different) = } ifelse
	   different
	   OPT> (abc) (abc) eq { (same) = } { (different) = } ifelse
	   same

	Python:

	.. code-block:: console

		>>> if 'abc' == 'efg':
		...   print ('same')
		... else:
		...   print ('different')
		different


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
Optscript executable in ctags assumes that all tags are in the corkQueue and
they have corkIndexes. See ":ref:`output-tag-stream`" about corkQueue and corkIndexes.

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

You can run optscript code fragments when pattern specified with
options matches successfully. The options are ``--regex-<LANG>``,
``--mline-regex-<LANG>``, and ``--_mtable-regex-<LANG>`` as you
expect. In addition, ``--_prelude-<LANG>`` and ``--_sequel-<LANG>``
options also take code fragments.

TBW: two timings of evaluation

Put code fragments at the end of options with surrounding "``{{``" and
"``}}``". Though it is not impossible, a command line is not suitable
place to write code fragments because the options with code fragments
may be too long. Instead, you should write them to your .ctags file.

.. warning:: An important rule in writing Optscript code in a .ctags
  file is the start marker, ``{{``, must be at the end of line, and the
  end marker, ``}}``, must be at the beginning of line. If you break the
  rule, the optlib loader of ctags fails to read your file.

``--_prelude-<LANG>`` is for specified code fragments run at the
beginning of parsing a source file. You can use this option for
defining the common code used in the parser.

``--_sequel-<LANG>`` is for for specified code fragments run at the end
of parser a source file. You can use this option for debug-printing
the final state of parsing the source file.
e.g. ``--_sequel-Foo={{ _traced { pstack } if }}``.

``--_list-operators`` lists all operators (and built-in procedures)
and exits. In additions to operators defined in  ``optscript``,
``ctags`` provides operators for tagging.

``OP`` column of ``--list-fields`` represents the availability of
operators for accessing the field specified in the line.  ``r``
represents the field has an operator for reading
(``:fieldname``). ``w`` represents the field has an operator for
writing (``fieldname:``).

``optlib2c``, a translator from .ctags file to C language source file
supports the code framents. Some of optlib parsers integrated to
Universal Ctags already use Optscript. You can find practical
examples of Optscript in files in ``optlib`` directory. The positional
rules about `{{` and `}}` is applicable to ``optlib2c``.

Data types
..........

Non-standard data types for tagging are added.  You can use it only in
``ctags`` command, not in ``optscript`` command.

``matchloc``

	This opaque data type is for representing a position in a source
	file. The name is an acronym for "match location".  ``_matchloc``
	pushes a ``matchloc`` object to ``ostack``.

``corkIndex:int`` or ``index:int``

	This represents a corkIndex. ``.`` pushes the corkIndex for a tag
	just created with the option owning the code frament.

``tag``

	This represents a tag data structure which is not in the corkQueue
	yet. ``_tag`` is an operator pushing a ``tag`` object to
	``ostack``. ``_commit`` puts the ``tag`` object on ``ostack`` to
	the corkQueue, and pushes an integer as a corkIndex.

Operators
............................

**.** -> ``-`` **.** ``corkIndex:int``

	Push the cork index for the tag

**\\n** -> ``-`` **\\n** ``matchedString:string``

	``n`` is an integer (1...9) representing a group in a pattern.
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

``_commit``

	TBW

``_traced``

	TBW


Recipes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Arrange the name of a tag
...........................................

"input.foo":

.. code-block::

	def a
	def b

Goal: If a language specific extra ``extendedName`` is given, the
parser for input.foo emits extra tags having ``X`` as prefix.

The base version of .ctags ("foo0.ctags"):

.. code-block:: ctags

	--langdef=foo
	--map-foo=.foo
	--kinddef-foo=d,definition,definitions
	--regex-foo=/^def[ \t]+([a-z]+)/\1/d/

The tags output ("output0.tags") for the base version
with "``--optoins=foo0.ctags input.foo``":

.. code-block:: tags

   a	input.foo	/^def a$/;"	d
   b	input.foo	/^def b$/;"	d

The Optscript version of .ctags ("foo1.ctags") for achieving the goal:

.. code-block:: ctags

	--langdef=foo
	--map-foo=.foo
	--kinddef-foo=d,definition,definitions
	--_extradef-foo=extendedName, tags prefixed with X
	--regex-foo=/^def[ \t]+([a-z]+)/\1/d/{{
	    /foo.extendedName _extraenabled {
	        mark \1 ?X _buildstring % name, \1 + 'X'
		    /definition             % kind
		    1 _matchloc             % location for \1
		    _tag                    % a tag object is pushed.
		    % ostack => tag
		    dup
		    % ostack => tag tag
		    /foo.extendedName _markextra
		    % ostack => tag
		    _commit
	    } if
	}}

The tags output ("output1.tags") for the Optscript version
with "``--optoins=foo1.ctags --extras-foo=+'{extendedName}' input.foo``":

.. code-block:: tags

   a	input.foo	/^def a$/;"	d
   aX	input.foo	/^def a$/;"	d
   b	input.foo	/^def b$/;"	d
   bX	input.foo	/^def b$/;"	d

Difference between Optscript and PostScript
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* Memory management

* Dynamically extendable data type

  - string

  - array
