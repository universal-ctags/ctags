.. _cxx:

======================================================================
The new C/C++ parser
======================================================================

:Maintainer: Szymon Tomasz Stefanek <s.stefanek@gmail.com>

Introduction
---------------------------------------------------------------------

The C++ language has strongly evolved since the old C/C++ parser was
written. The old parser was struggling with some of the new features
of the language and has shown signs of reaching its limits. For this
reason in February/March 2016 the C/C++ parser was rewritten from
scratch.

In the first release several outstanding bugs were fixed and some new
features were added. Among them:

- Tagging of "using namespace" declarations
- Tagging of function parameters
- Extraction of function parameter types
- Tagging of anonymous structures/unions/classes/enums
- Support for C++11 lambdas (as anonymous functions)
- Support for function-level scopes (for local variables and parameters)
- Extraction of local variables which include calls to constructors
- Extraction of local variables from within the for(), while(), if()
  and switch() parentheses.
- Support for function prototypes/declarations with trailing return type

At the time of writing (March 2016) more features are planned.

Notable New Features
---------------------------------------------------------------------

Some of the notable new features are described below.

Properties
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Several properties of functions and variables can be extracted
and placed in a new field called ``properties``.
The syntax to enable it is:

.. code-block:: console

	$ ctags ... --fields-c++=+{properties} ...

At the time of writing the following properties are reported:

- ``virtual``: a function is marked as virtual
- ``static``: a function/variable is marked as static
- ``inline``: a function implementation is marked as inline
- ``explicit``: a function is marked as explicit
- ``extern``: a function/variable is marked as extern
- ``const``: a function is marked as const
- ``pure``: a virtual function is pure (i.e = 0)
- ``override``: a function is marked as override
- ``default``: a function is marked as default
- ``final``: a function is marked as final
- ``delete``: a function is marked as delete
- ``mutable``: a variable is marked as mutable
- ``volatile``: a function is marked as volatile
- ``specialization``: a function is a template specialization
- ``scopespecialization``: template specialization of scope ``a<x>::b()``
- ``deprecated``: a function is marked as deprecated via ``__attribute__``
- ``scopedenum``: a scoped enumeration (C++11)

Preprocessor macros
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Defining a macro from command line
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The new parser supports the definition of real preprocessor macros
via the ``-D`` option. All types of macros are supported,
including the ones with parameters and variable arguments.
Stringification, token pasting and recursive macro expansion are also supported.

Option ``-I`` is now simply a backward-compatible syntax to define a
macro with no replacement.

The syntax is similar to the corresponding gcc ``-D`` option.

Some examples follow.

.. code-block:: console

	$ ctags ... -D IGNORE_THIS ...

With this commandline the following C/C++ input

.. code-block:: C

	int IGNORE_THIS a;

will be processed as if it was

.. code-block:: C

	int a;

Defining a macro with parameters uses the following syntax:

.. code-block:: console

	$ ctags ... -D "foreach(arg)=for(arg;;)" ...

This example defines ``for(arg;;)`` as the replacement ``foreach(arg)``.
So the following C/C++ input

.. code-block:: C

	foreach(char * p,pointers)
	{

	}

is processed in new C/C++ parser as:

.. code-block:: C

	for(char * p;;)
	{

	}

and the p local variable can be extracted.

The previous commandline includes quotes since the macros generally contain
characters that are treated specially by the shells. You may need some escaping.

Token pasting is performed by the ``##`` operator, just like in the normal
C preprocessor.

.. code-block:: console

	$ ctags ... -D "DECLARE_FUNCTION(prefix)=int prefix ## Call();"

So the following code

.. code-block:: C

	DECLARE_FUNCTION(a)
	DECLARE_FUNCTION(b)

will be processed as

.. code-block:: C

	int aCall();
	int bCall();

Macros with variable arguments use the gcc ``__VA_ARGS__`` syntax.

.. code-block:: console

	$ ctags ... -D "DECLARE_FUNCTION(name,...)=int name(__VA_ARGS__);"

So the following code

.. code-block:: C

	DECLARE_FUNCTION(x,int a,int b)

will be processed as

.. code-block:: C

	int x(int a,int b);

Automatically expanding macros defined in the same input file (HIGHLY EXPERIMENTAL)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a CPreProcessor macro defined in a C/C++/CUDA file, the macro invocation in the
SAME file can be expanded with following options:

.. code-block:: text

   --param-CPreProcessor._expand=1
   --fields-C=+{macrodef}
   --fields-C++=+{macrodef}
   --fields-CUDA=+{macrodef}
   --fields=+{signature}

Let's see an example.

input.c:
.. code-block:: C

	#define DEFUN(NAME) int NAME (int x, int y)
	#define BEGIN {
	#define END }

	DEFUN(myfunc)
	  BEGIN
	  return -1
	  END

The output without options:
.. code-block::

   $ ctags -o - input.c
   BEGIN	input.c	/^#define BEGIN /;"	d	language:C	file:
   DEFUN	input.c	/^#define DEFUN(/;"	d	language:C	file:
   END	input.c	/^#define END /;"	d	language:C	file:

The output with options:
.. code-block::

   $ ctags --param-CPreProcessor._expand=1 --fields-C=+'{macrodef}' --fields=+'{signature}' -o - input.c
   BEGIN	input.c	/^#define BEGIN /;"	d	language:C	file:	macrodef:{
   DEFUN	input.c	/^#define DEFUN(/;"	d	language:C	file:	signature:(NAME)	macrodef:int NAME (int x, int y)
   END	input.c	/^#define END /;"	d	language:C	file:	macrodef:}
   myfunc	input.c	/^DEFUN(myfunc)$/;"	f	language:C	typeref:typename:int	signature:(int x,int y)

``myfunc`` coded by ``DEFUN`` macro is captured well.


This feature is highly experimental. At least three limitations are known.

* This feature doesn't understand ``#undef`` yet.
  Once a macro is defined, its invocation is always expanded even
  after the parser sees ``#undef`` for the macro in the same input
  file.

* Macros are expanded incorrectly if the result of macro expansion
  includes the macro invocation again.

* Currently, ctags can expand a macro invocation only if its
  definitions are in the same input file. ctags cannot expand a macro
  defined in the header file included from the current input file.

Enabling this macro expansion feature makes the parsing speed about
two times slower.


Incompatible Changes
---------------------------------------------------------------------

The parser is mostly compatible with the old one. There are some minor
incompatible changes which are described below.


Anonymous structure names
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The old parser produced structure names in the form ``__anonN`` where N
was a number starting at 1 in each file and increasing at each new
structure. This caused collisions in symbol names when ctags was run
on multiple files.

In the new parser the anonymous structure names depend on the file name
being processed and on the type of the structure itself. Collisions are
far less likely (though not impossible as hash functions are unavoidably
imperfect).

Pitfall: the file name used for hashing includes the path as passed to the
ctags executable. So the same file "seen" from different paths will produce
different structure names. This is unavoidable and is up to the user to
ensure that multiple ctags runs are started from a common directory root.

File scope
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The file scope information is not 100% reliable. It never was.
There are several cases in that compiler, linker or even source code
tricks can "unhide" file scope symbols (for instance \*.c files can be
included into each other) and several other cases in that the limitation
of the scope of a symbol to a single file simply cannot be determined
with a single pass or without looking at a program as a whole.

The new parser defines a simple policy for file scope association
that tries to be as compatible as possible with the old parser and
should reflect the most common usages. The policy is the following:

- Namespaces are in file scope if declared inside a .c or .cpp file

- Function prototypes are in file scope if declared inside a .c or .cpp file

- K&R style function definitions are in file scope if declared static
  inside a .c file.

- Function definitions appearing inside a namespace are in file scope only
  if declared static inside a .c or .cpp file.
  Note that this rule includes both global functions (global namespace)
  and class/struct/union members defined outside of the class/struct/union
  declaration.

- Function definitions appearing inside a class/struct/union declaration
  are in file scope only if declared static inside a .cpp file

- Function parameters are always in file scope

- Local variables are always in file scope

- Variables appearing inside a namespace are in file scope only if
  they are declared static inside a .c or .cpp file

- Variables that are members of a class/struct/union are in file scope
  only if declared in a .c or .cpp file

- Typedefs are in file scope if appearing inside a .c or .cpp file

Most of these rules are debatable in one way or the other. Just keep in mind
that this is not 100% reliable.

Inheritance information
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The new parser does not strip template names from base classes.
For a declaration like

.. code-block:: C

	template<typename A> class B : public C<A>

the old parser reported ``C`` as base class while the new one reports
``C<A>``.

Typeref
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The syntax of the typeref field (``typeref:A:B``) was designed with only
struct/class/union/enum types in mind. Generic types don't have ``A``
information and the keywords became entirely optional in C++:
you just can't tell. Furthermore, struct/class/union/enum types
share the same namespace and their names can't collide, so the ``A``
information is redundant for most purposes.

To accommodate generic types and preserve some degree of backward
compatibility the new parser uses struct/class/union/enum in place
of ``A`` where such keyword can be inferred. Where the information is
not available it uses the 'typename' keyword.

Generally, you should ignore the information in field ``A`` and use
only information in field ``B``.
