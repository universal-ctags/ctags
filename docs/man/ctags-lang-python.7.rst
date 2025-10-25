.. _ctags-lang-python(7):

==============================================================
ctags-lang-python
==============================================================

Random notes about tagging python source code with Universal Ctags

:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Python ...
|	**ctags** ... --language-force=Python ...
|	**ctags** ... --map-Python=+.py ...

DESCRIPTION
-----------
This man page gathers random notes about tagging python source code.

TAGGING ``import`` STATEMENTS
-----------------------------

Summary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`import X`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	X    module     imported           N/A
	==== ========== ================== ===================

`import X as Y`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	X    module     indirectlyImported N/A
	Y    namespace  definition         nameref:module:X
	==== ========== ================== ===================

`from X import *`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	`X`  module     namespace          N/A
	==== ========== ================== ===================

`from X import Y`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	`X`  module     namespace          N/A
	`Y`  unknown    imported           scope:module:`X`
	==== ========== ================== ===================

`from X import Y as Z`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	`X`  module     namespace          N/A
	`Y`  unknown    indirectlyImported scope:module:`X`
	`Z`  unknown    definition         nameref:unknown:`Y`
	==== ========== ================== ===================

..
	===================== ==== ========== ================== ===================
	input code            name kind       role               other noticeable fields
	===================== ==== ========== ================== ===================
	import X              X    module     imported
	import X as Y         X    module     indirectlyImported
	import X as Y         Y    namespace  definition         nameref:module:X
	from X import *       X    module     namespace
	from X import Y       X    module     namespace
	from X import Y       Y    unknown    imported           scope:module:X
	from X import Y as Z  X    module     namespace
	from X import Y as Z  Y    unknown    indirectlyImported scope:module:X
	from X import Y as Z  Z    unknown    definition         nameref:unknown:Y
	===================== ==== ========== ================== ===================

..  a table having merged cells cannot be converted to man page
..
	+--------------------+------------------------------------------------------+
	|input code          |output tags                                           |
	|                    +----+----------+------------------+-------------------+
	|                    |name| kind     |role              |other noticeable fields  |
	+====================+====+==========+==================+===================+
	|import X            |X   | module   |imported          |                   |
	+--------------------+----+----------+------------------+-------------------+
	|import X as Y       |X   | module   |indirectlyImported|                   |
	|                    +----+----------+------------------+-------------------+
	|                    |Y   | namespace|definition        |nameref:module:X   |
	+--------------------+----+----------+------------------+-------------------+
	|from X import *     |X   | module   |namespace         |                   |
	+--------------------+----+----------+------------------+-------------------+
	|from X import Y     |X   | module   |namespace         |                   |
	|                    +----+----------+------------------+-------------------+
	|                    |Y   | unknown  |imported          |scope:module:X     |
	+--------------------+----+----------+------------------+-------------------+
	|from X import Y as Z|X   | module   |namespace         |                   |
	|                    +----+----------+------------------+-------------------+
	|                    |Y   | unknown  |indirectlyImported|scope:module:X     |
	|                    +----+----------+------------------+-------------------+
	|                    |Z   | unknown  |definition        |nameref:unknown:Y  |
	+--------------------+----+----------+------------------+-------------------+

Examples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"input.py"

.. code-block:: Python

   import X0

"output.tags"
with "--options=NONE -o - --extras=+r --fields=+rzK input.py"

.. code-block:: tags

	X0	input.py	/^import X0$/;"	kind:module	roles:imported

A tag for an imported module has ``module`` kind with ``imported`` role.  The
module is not defined here; it is defined in another file. So the tag for the
imported module is a reference tag; specify ``--extras=+r`` (or
``--extras=+{reference}``) option for tagging it.  "roles:" field enabled with
``--fields=+r`` is for recording the module is "imported" to the tag file.

"input.py"

.. code-block:: Python

	import X1 as Y1

"output.tags"
with "--options=NONE -o - --extras=+r --fields=+rzK --fields-Python=+{nameref} input.py"

.. code-block:: tags

	X1	input.py	/^import X1 as Y1$/;"	kind:module	roles:indirectlyImported
	Y1	input.py	/^import X1 as Y1$/;"	kind:namespace	roles:def	nameref:module:X1

"Y1" introduces a new name and is defined here. So "Y1" is tagged as a
definition tag.  "X1" is imported in a way that its name cannot be used
in this source file. For letting client tools know that the name cannot be used,
``indirectlyImported`` role is assigned for "X1".  "Y1" is the name for
accessing objects defined in the module imported via "X1".  For recording this
relationship, ``nameref:`` field is attached to the tag of "Y1".  Instead of
``module`` kind, ``namespace`` kind is assigned to "Y1" because "Y1" itself
isn't a module.

"input.py"

.. code-block:: Python

	from X2 import *

"output.tags"
with "--options=NONE -o - --extras=+r --fields=+rzK input.py"

.. code-block:: tags

	X2	input.py	/^from X2 import *$/;"	kind:module	roles:namespace

The module is not defined here; it is defined in another file. So the tag for
the imported module is a reference tag. Unlike "X0" in "import X0", "X2" may not
be used because the names defined in "X2" can be used in this source file. To represent
the difference ``namespace`` role is attached to "X2" instead of ``imported``.

"input.py"

.. code-block:: Python

	from X3 import Y3

"output.tags"
with "--options=NONE -o - --extras=+r --fields=+rzKZ input.py"

.. code-block:: tags

	X3	input.py	/^from X3 import Y3$/;"	kind:module	roles:namespace
	Y3	input.py	/^from X3 import Y3$/;"	kind:unknown	scope:module:X3	roles:imported

"Y3" is a name for a language object defined in "X3" module. "scope:module:X3"
attached to "Y3" represents this relation between "Y3" and "X3". ctags
assigns ``unknown`` kind to "Y3" because ctags cannot know whether "Y3" is a
class, a variable, or a function from the input file.

"input.py"

.. code-block:: Python

	from X4 import Y4 as Z4

"output.tags"
with "--options=NONE -o - --extras=+r --fields=+rzKZ input.py"

.. code-block:: tags

	X4	input.py	/^from X4 import Y4 as Z4$/;"	kind:module	roles:namespace
	Y4	input.py	/^from X4 import Y4 as Z4$/;"	kind:unknown	scope:module:X4	roles:indirectlyImported
	Z4	input.py	/^from X4 import Y4 as Z4$/;"	kind:unknown	roles:def	nameref:unknown:Y4

"Y4" is similar to "Y3" of "from X3 import Y3" but the name cannot be used here.
``indirectlyImported`` role assigned to "Y4" representing this. "Z4" is the name for
accessing the language object named in "Y4" in "X4" module. "nameref:unknown:Y4"
attached to "Z4" and "scope:module:X4" attached to "Y4" represent the relations.

LAMBDA EXPRESSION AND TYPE HINT
-------------------------------

Summary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`id = lambda var0: var0`

	=========== ========== ================== ===================
	name        kind       role               other noticeable fields
	=========== ========== ================== ===================
	`id`        function   definition         signature:(`var0`)
	=========== ========== ================== ===================

`id_t: Callable[[int], int] = lambda var1: var1`

	=========== ========== ================== ===================
	name        kind       role               other noticeable fields
	=========== ========== ================== ===================
	`id_t`      variable   definition         typeref:typename:`Callable[[int], int]` nameref:function:anonFuncN
	anonFuncN   function   definition         signature:(`var1`)
	=========== ========== ================== ===================

Examples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"input.py"

.. code-block:: Python

	from typing import Callable
	id = lambda var0: var0
	id_t: Callable[[int], int] = lambda var1: var1

"output.tags"
with "--options=NONE -o - --sort=no --fields=+KS --fields-Python=+{nameref} --extras=+{anonymous} input.py"

.. code-block:: tags

	id	input.py	/^id = lambda var0: var0$/;"	function	signature:(var0)
	id_t	input.py	/^id_t: Callable[[int], int] = lambda var1: var1$/;"\
		variable	typeref:typename:Callable[[int], int]	nameref:function:anonFunc84011d2c0101
	anonFunc84011d2c0101	input.py	/^id_t: Callable[[int], int] = lambda var1: var1$/;"\
		function	signature:(var1)

If a variable ("id") with no type hint is initialized with a lambda expression,
ctags assigns ``function`` kind for the tag of "id".

If a variable ("id_t") with a type hint is initialized with a lambda expression,
ctags assigns ``variable`` kind for the tag of "id_t" with ``typeref:`` and
``nameref:`` fields. ctags fills ``typeref:`` field with the value of the type
hint. The way of filling ``nameref:`` is a bit complicated.

For the lambda expression used in initializing the type-hint'ed variable, ctags
creates ``anonymous`` extra tag ("anonFunc84011d2c0101"). ctags fills the
``nameref:`` field of "id_t" with the name of ``anonymous`` extra tag:
"nameref:function:anonFunc84011d2c0101".

You may think why ctags does so complicated, and why ctags doesn't emit
following tags output for the input::

	id	input.py	/^id = \\$/;"	function	signature:(var0)
	id_t	input.py	/^id_t: \\$/;"	function	typeref:typename:Callable[[int], int]	signature:(var1)

There is a reason. The other languages of ctags obey the following rule: ctags fills
``typeref:`` field for a tag of a callable object (like function) with the type
of its return value. If we consider "id_t" is a function, its ``typeref:`` field
should have "typename:int". However, for filling ``typeref:`` with "typename:int",
ctags has to analyze "Callable[[int], int]" deeper. We don't want to do so.

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* New role ``entryPoint`` for ``module`` kind

  PythonEntryPoints parser emits tag entries having this role.

* New role ``entryPoint`` for  ``function`` kind

  PythonEntryPoints parser emits tag entries having this role.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`, :ref:`ctags-lang-iPythonCell(7) <ctags-lang-iPythonCell(7)>`
