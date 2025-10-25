.. _ctags-lang-gdscript(7):

==============================================================
ctags-lang-gdscript
==============================================================

Random notes about tagging GDScript source code with Universal Ctags

:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+GDScript ...
|	**ctags** ... --language-force=GDScript ...
|	**ctags** ... --map-GDScript=+.gd ...

DESCRIPTION
-----------
This man page gathers random notes about tagging GDScript source code
with Universal Ctags.

Storing Annotations
-------------------
Like the Python parser storing decorations to ``decorations`` field,
the GDScript parser stores annotations
starting from `@` to the language specific field, ``annotations``.
Though the field is enabled explicitly in following examples, the
field is enabled by default.

"input.gd"

.. code-block:: GDScript

	@export
	var s = "Hello"

	@master
	func f(msg):
		print(msg)

"output.tags"
with "--options=NONE --sort=no --fields-GDScript=+{annotations} -o - input.gd"

.. code-block:: tags

	s	input.gd	/^var s = "Hello"$/;"	v	annotations:export
	f	input.gd	/^func f(msg):$/;"	m	annotations:master

Extracting `func`
-----------------
A language object defined with `func` keyword is tagged with ``method`` kind.
Like annotations, the parser stores keywords modifying `func` like `static` to
the ``annotations`` field.

"input.gd"

.. code-block:: GDScript

	func f(x):
		return x

	static func f_s(x):
		reutrn x

	remote func f_r(x):
		return x


"output.tags"
with "--options=NONE --sort=no --fields=+K --fields-GDScript=+{annotations} -o - input.gd"

.. code-block:: tags

	f	input.gd	/^func f(x):$/;"	method
	f_s	input.gd	/^static func f_s(x):$/;"	method	annotations:static
	f_r	input.gd	/^remote func f_r(x):$/;"	method	annotations:remote

Tagging implicitly defined classes
----------------------------------
"A file is a class!" in GDScript.  A class is implicitly
defined. Functions, variables, constants, and signals are parts of the
class though the class is unnamed by default.

If the language specific extra, ``implicitClass``, is enabled, the
parser makes a anonymous tag for the class. The parser fills the scope
fields of the tags for all language objects defined in the file with
the anonymous tag.

Let's see an example demonstrating the effect of the extra.

Turning off the extra:

"input.gd"

.. code-block:: GDScript

	func f(x):
		return x

"output.tags"
with "--options=NONE --fields=+KZ --extras-GDScript=-{implicitClass} -o - input.gd"

.. code-block:: tags

	f	input.gd	/^func f(x):$/;"	method

Turning on the extra:

"input.gd"

.. code-block:: GDScript

	func g(x):
		return x

"output.tags"
with "--options=NONE --fields=+KZ --extras-GDScript=+{implicitClass} -o - input.gd"

.. code-block:: tags

	anon_class_84011bee0100	input.gd	/^func g(x):$/;"	class
	g	input.gd	/^func g(x):$/;"	method	scope:class:anon_class_84011bee0100

Tagging the name specified with `class_name`
---------------------------------------------
`class_name` is a keyword for giving a name to the implicitly defined
class.  If ``implicitClass`` is turned off, the parser just extract
the name coming after the keyword with ``class`` kind. If
``implicitClass`` is turned on, the parser converts the anonymous tag
to a non-anonymous tag with the specified name.  When converting,
the parser also updates scope fields of the other tags in the file.

Turning off the extra:

"input.gd"

.. code-block:: GDScript

	class_name c

	func f(x):
		return x

"output.tags"
with "--options=NONE --fields=+KZ --extras-GDScript=-{implicitClass} -o - input.gd"

.. code-block:: tags

	c	input.gd	/^class_name c$/;"	class
	f	input.gd	/^func f(x):$/;"	method

Turning on the extra:

"input.gd"

.. code-block:: GDScript

	class_name C
	func g(x):
		return x

"output.tags"
with "--options=NONE --fields=+KZ --extras-GDScript=+{implicitClass} -o - input.gd"

.. code-block:: tags

	C	input.gd	/^class_name C$/;"	class
	g	input.gd	/^func g(x):$/;"	method	scope:class:C

Filling ``inherits`` field
--------------------------
`extends` keyword specifies the super class for the implicitly defined class.
If `implicitClass` extra is turned on, the parser fills ``inherits`` field
of the tag for the implicitly defined class with the name of super class.

"input.gd"

.. code-block:: GDScript

	extends B
	class_name C

"output.tags"
with "--options=NONE --fields=+Ki --extras-GDScript=+{implicitClass} -o - input.gd"

.. code-block:: tags

	C	input.gd	/^class_name C$/;"	class	inherits:B

When `--extras=+r` is given, the parser extracts the class specified with the
`extends` keyword as a reference tag of ``class`` kind with ``extended`` role.

"input.gd"

.. code-block:: GDScript

	extends B

"output.tags"
with "--options=NONE --fields=+rEK --extras=+r -o - input.gd"

.. code-block:: tags

	B	input.gd	/^extends B$/;"	class	roles:extended	extras:reference

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`
