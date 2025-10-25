.. _ctags-lang-julia(7):

==============================================================
ctags-lang-julia
==============================================================

Random notes about tagging Julia source code with Universal-ctags

:Version: 6.2.1
:Manual group: Universal-ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Julia ...
|	**ctags** ... --language-force=Julia ...
|	**ctags** ... --map-Julia=+.jl ...

DESCRIPTION
-----------
This man page gathers random notes about tagging Julia source code.

TAGGING ``import`` AND ``using`` EXPRESSIONS
--------------------------------------------

Summary
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`using X`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	X    module     used               N/A
	==== ========== ================== ===================

`using X: a, b`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	X    module     namespace          N/A
	a, b unknown    used               scope:module:X
	==== ========== ================== ===================

`import X`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	X    module     imported           N/A
	==== ========== ================== ===================

`import X.a, Y.b`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	X, Y module     namespace          N/A
	a    unknown    imported           scope:module:X
	b    unknown    imported           scope:module:Y
	==== ========== ================== ===================

`import X: a, b`

	==== ========== ================== ===================
	name kind       role               other noticeable fields
	==== ========== ================== ===================
	X    module     namespace          N/A
	a,b  unknown    imported           scope:module:X
	==== ========== ================== ===================

Examples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"input.jl"

.. code-block:: Julia

	using X0

"output.tags"
with "--options=NONE -o - --extras=+r --fields=+rzK input.jl"

.. code-block:: tags

	X0	input.jl	/^using X0$/;"	kind:module	roles:used

``--extras=+r`` (or ``--extras=+{reference}``) option is needed for this tag,
since it's a reference tag. This is because module ``X`` is not defined here.
It is defined in another file. Enable ``roles:`` field with ``--fields=+r`` is
for recording that the module is "used", i.e., loaded by ``using``.

"input.jl"

.. code-block:: Julia

	import X1.a, X2.b, X3

"output.tags"
with "--options=NONE -o - --extras=+r --fields=+rzKZ input.jl"

.. code-block:: tags

	X1	input.jl	/^import X1.a, X2.b, X3$/;"	kind:module	roles:namespace
	X2	input.jl	/^import X1.a, X2.b, X3$/;"	kind:module	roles:namespace
	X3	input.jl	/^import X1.a, X2.b, X3$/;"	kind:module	roles:imported
	a	input.jl	/^import X1.a, X2.b, X3$/;"	kind:unknown	scope:module:X1	roles:imported
	b	input.jl	/^import X1.a, X2.b, X3$/;"	kind:unknown	scope:module:X2	roles:imported

Why ``X1`` and ``X2`` have role "namespace", while ``X3`` have role "imported"?
It's because the symbol ``a`` in module ``X1``, and ``b`` in module ``X2`` are
brought to the current scope, but ``X1`` and ``X2`` themselves are not. We use
"namespace" role for such modules.

``X3`` is different. The symbol ``X3``, together with all exported symbols in
``X3``, is brought to current scope. For such modules, we use "imported" or
"used" role depending whether they are loaded by ``import`` or ``using``.

Also, notice that ``a`` and ``b`` have the "unknown" kind. This is because we
cannot know whether it's a function, constant, or macro, etc.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`
