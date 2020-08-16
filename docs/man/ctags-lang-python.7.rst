.. _ctags-lang-python(7):

==============================================================
ctags-lang-python
==============================================================
-------------------------------------------------------------------
Random notes about tagging python source code with Universal-ctags
-------------------------------------------------------------------
:Version: 0.0.0
:Manual group: Universal-ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Python ...
|	**ctags** ... --language-force=Python ...
|	**ctags** ... --map-Python=+.py ...

DESCRIPTION
-----------
This man page gathers random notes about tagging python source code.

EXAMPLES
--------
This section shows how ctags uses kinds, roles, fields, and extras when tagging
python source code for given input.

Tagging ``import`` statements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
with "--options=NONE -o - --extras=+r --fields=+rzK --fields-Python='+{nameref}' input.py"
::

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
::

	X2	input.py	/^from X2 import \*$/;"	kind:module	roles:namespace

The module is not defined here; it is defined in another file. So the tag for
the imported module is a reference tag. Unlike "X0" in "import X0", "X2" may not
be used because the names defined in "X2" can be used in this source file. To represent
the difference ``namespace`` role is attached to "X2" instead of ``imported``.

"input.py"
.. code-block:: Python

	from X3 import Y3

"output.tags"
with "--options=NONE -o - --extras=+r --fields=+rzKZ input.py"
::

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
::

	X4	input.py	/^from X4 import Y4 as Z4$/;"	kind:module	roles:namespace
	Y4	input.py	/^from X4 import Y4 as Z4$/;"	kind:unknown	scope:module:X4	roles:indirectlyImported
	Z4	input.py	/^from X4 import Y4 as Z4$/;"	kind:unknown	roles:def	nameref:unknown:Y4

"Y4" is similar to "Y3" of "from X3 import Y3" but the name cannot be used here.
``indirectlyImported`` role assigned to "Y4" representing this. "Z4" is the name for
accessing the language object named in "Y4" in "X4" module. "nameref:unknown:Y4"
attached to "Z4" and "scope:module:X4" attached to "Y4" represent the relations.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`
