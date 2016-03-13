The new C/C++ parser
=====================================================================

:Maintainer: Szymon Tomasz Stefanek <s.stefanek@gmail.com>

Introduction
---------------------------------------------------------------------

The C++ language has strongly evolved since the old C/C++ parser was
written. The old parser was struggling with some of the new features
of the language and hash shown signs of reaching its limits.
For this reason in february/march 2016 the C/C++ parser has been
rewritten from scratch.

In the first release several outstanding bugs were fixed and some new
features have been added. Among them:
- Tagging of "using namespace" declarations
- Tagging of function parameters
- Extraction of function parameter types
- Tagging of anonymous structures/unions/classes/enums
- Support for C++11 lambdas (as anonymous functions)
- Support for function-level scopes (for local variables and parameters)
- Extraction of local variables which include calls to constructors
- Extraction of local variables from within the for(), while(), if()
  and switch() parentheses.
- Support of function prototypes/declarations with trailing return type

At the time of writing (March 2016) more features are planned.

The parser is mostly compatible with the old one. There are some minor
incompatible changes which are described below.

Anonymous structure names
---------------------------------------------------------------------

The old parser produced structure names in the form __anonN where N
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
---------------------------------------------------------------------

The file scope information is not 100% reliable. It never was.
There are several cases in that compiler, linker or even source code
tricks can "unhide" file scope symbols (for instance *.c files can be
included into each other) and several other cases in that the limitation
of the scope of a symbol to a single file simply cannot be determined
with a single pass or without looking at a program as a whole.

The new parser defines a simple policy for file scope association
that tries to be as compatible as possible with the old parser ad should
reflect the most common usages. The policy is the following:

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
that s is not 100% reliable.

Inheritance information
---------------------------------------------------------------------

The new parser does not strip template names from base classes.
For a declaration like

template<typename A> class B : public C<A>

the old parser reported "C" as base class while the new one reports
"C<A>".


