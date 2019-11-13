..
    NOT REVIEWED YET

======================================================================
Request for extending a parser (or Reporting a bug of parser)
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
    :depth: 3
    :local:

----

Sometimes you will find u-ctags doesn't make a tag for a language
object unexpectedly. Writing a patch for making the tag is
appreciate. However, you may not have time to do so. In that case, you
can open an issue on the GitHub page of u-ctags.

This section tells you how to drive u-ctags developers effectively.

Before reporting
---------------------------------------------------------------------

U-Ctags just captures the definitions of language objects.  U-ctags
has an infrastructure for capturing references for language objects.
However, we implement reference tagging limited area.  We will not
work on writing new code for capturing references for your favorite
language.  About requests for capturing reference tags, we will say
"patches are welcome.".

What kind of language objects u-ctags captures is controlled by
`--kind-<LANG>` option. Some kinds are disabled by default because we
assume people don't want too large `tags` file. When you cannot find a
language object you want in a tags file, it is worth for checking the
status of kinds. `--list-kinds=<LANG>` or (`--list-kinds-full=<LANG>`)
option lists the status of the given language.

Let's see an example.

Consider following input (foo.h):

.. code-block:: C

    struct point {
      int x, y;
    };

    struct point *make_point(int x0, int y0);

tags output generated with `u-ctags -o - /tmp/foo.h` is as following.
::

    point    foo.h    /^struct point {$/;"    s
    x    foo.h    /^  int x, y;$/;"    m    struct:point    typeref:typename:int
    y    foo.h    /^  int x, y;$/;"    m    struct:point    typeref:typename:int

Though `point`, `x` and `y` are tagged, the declaration `make_point`
is not tagged because `prototype` kind of C++ is disabled by default.
You can know it from the output of `ctags --list-kinds-full=C++`.
::

    #LETTER NAME       ENABLED REFONLY NROLES MASTER DESCRIPTION
    A       alias      no      no      0      NONE   namespace aliases
    L       label      no      no      0      C      goto labels
    N       name       no      no      0      NONE   names imported via using scope::symbol
    ...
    p       prototype  no      no      0      C      function prototypes

By turning on the kind with `--kinds-C++=+p`, u-ctags tags
`make_point`::

    make_point    foo.h    /^struct point *make_point(int x0, int y0);$/;"    p    typeref:struct:point *
    point    foo.h    /^struct point {$/;"    s
    x    foo.h    /^  int x, y;$/;"    m    struct:point    typeref:typename:int
    y    foo.h    /^  int x, y;$/;"    m    struct:point    typeref:typename:int

Wildcard `*` is for enabling all kinds of a language at once.
`--kinds-C++=*` option enables all kinds of C++ parser. If you specify `all`
as the name of `<LANG>`, you can enable all kinds of all languages at once.

The content of report
---------------------------------------------------------------------

Don't assume following three things.

U-ctags developers know vi.

    If you explain the expectation about how tags related functions of vi
    and its plugins, U-ctags developers don't understand it.
    The answer from them can be "it can be a bug of vi."

U-ctags developers know the programming language that you are talking.

    U-ctags developers need your help to understand the meaning of
    language object you asked tagging especially about kind.  A person
    extending a parser have to decide a kind of newly tagging language
    object: reusing an existing kind or introducing a new kind.
    U-ctags developers expect a report know the concept kind, field,
    and extra. ctags.1 man page of u-ctags explains them.

English is the native language of the head maintainer.

    I don't want to write this but I have to write this.
    Following are my private request for reporters.

    Instead of long explanation, showing code or output
    examples make me understand what you want.

    Don't omit sentences. Please, write your sentence
    in full.

    Use pronounce fewer.

U-ctags can generate something meaningful from a broken input.

    From garbage, u-ctags generates garbage.
    For a syntactically broken input source file, U-ctags
    does not work well. U-ctags developers will not work
    on improving u-ctags for handing such input.
    The exception is that macro related input. Well known
    one is C and C++.

Following a tuple with three items helps us to understand what you want.

1. Input file

    A shorter input file is better. However, it must be syntactically
    valid.  Show the URL (or something) where you get the input
    file. It is needed to incorporate the input file to the u-ctags
    source tree as a test case.

2. Command line running u-ctags

3. Expected output

These three items should be rendered preformatted form on an issue
page of GitHub. Use triple backquotes notation of GitHub's
markdown notation. I will close an issue with a bad notation
like this `issue <https://github.com/universal-ctags/ctags/issues/1547>`_.

An example of good report
---------------------------------------------------------------------

For the following input file(input.f90), u-ctags reports incomplete pattern
for function `f` at the line 23.

::

   ! input.f90, taken from https://github.com/universal-ctags/ctags/issues/1616
   module example_mod

    ! This module contains two interfaces:
    !   1. f_interface, which is an interface to the local f function
    !   2. g, which is implemented in the example_smod submodule

       interface f_interface
          ! The function `f` is defined below, within the `contains` statement
           module function f(x) result(y)
              integer :: x, y
           end function f
        end interface f_interface

       interface
          ! The function `g` is implemented in example_smod.f90
           module function g(x) result(y)
              integer :: x,y
           end function g
       end interface

       contains
        function f(x) result(y)
           integer :: x, y

           y = x * 2
        end function f
   end module example_mod

I run ctags with following command line::

  u-ctags --fields=+n -o - /tmp/input.f90

What I got::

	example_mod	/tmp/input.f90	/^module example_mod$/;"	m	line:2
	f	/tmp/input.f90	/^     fu/;"	f	line:23
	f_interface	/tmp/input.f90	/^    interface f_interface$/;"	i	line:8	module:example_mod

I think this should be::

	example_mod	/tmp/input.f90	/^module example_mod$/;"	m	line:2
	f	/tmp/input.f90	/^     function f/;"	f	line:23
	f_interface	/tmp/input.f90	/^    interface f_interface$/;"	i	line:8	module:example_mod

or::

	example_mod	/tmp/input.f90	/^module example_mod$/;"	m	line:2
	f	/tmp/input.f90	/^     function f(x) result(y)/;"	f	line:23
	f_interface	/tmp/input.f90	/^    interface f_interface$/;"	i	line:8	module:example_mod


Either way, `/^     fu/` is too short as a pattern.

The version of u-ctags is `83b0d1f6`::

	$ u-ctags --version
	Universal Ctags 0.0.0(83b0d1f6), Copyright (C) 2015 Universal Ctags Team
	Universal Ctags is derived from Exuberant Ctags.
	Exuberant Ctags 5.8, Copyright (C) 1996-2009 Darren Hiebert
	  Compiled: Dec 15 2017, 08:07:36
	  URL: https://ctags.io/
	  Optional compiled features: +wildcards, +regex, +multibyte, +debug, +option-directory, +xpath, +json, +interactive, +sandbox, +yaml
