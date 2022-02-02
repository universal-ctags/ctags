.. _optlib:

Extending ctags with Regex parser (*optlib*)
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

.. contents:: `Table of contents`
	:depth: 3
	:local:

.. TODO:
	add a section on debugging

Exuberant Ctags allows a user to add a new parser to ctags with ``--langdef=<LANG>``
and ``--regex-<LANG>=...`` options.
Universal Ctags follows and extends the design of Exuberant Ctags in more
powerful ways and call the feature as *optlib parser*, which is described in in
:ref:`ctags-optlib(7) <ctags-optlib(7)>` and the following sections.

:ref:`ctags-optlib(7) <ctags-optlib(7)>` is the primary document of the optlib
parser feature. The following sections provide additional information and more
advanced features. Note that some of the features are experimental, and will be
marked as such in the documentation.

Lots of optlib parsers are included in Universal Ctags,
`optlib/*.ctags <https://github.com/universal-ctags/ctags/tree/master/optlib>`_.
They will be good examples when you develop your own parsers.

A optlib parser can be translated into C source code. Your optlib parser can
thus easily become a built-in parser. See ":ref:`optlib2c`" for details.

Regular expression (regex) engine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Universal Ctags uses `the POSIX Extended Regular Expressions (ERE)
<https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html>`_
syntax as same as Exuberant Ctags by default.

During building Universal Ctags the ``configure`` script runs compatibility
tests of the regex engine in the system library.  If tests pass the engine is
used, otherwise the regex engine imported from `the GNU Gnulib library
<https://www.gnu.org/software/gnulib/manual/gnulib.html#Regular-expressions>`_
is used. In the latter case, ``ctags --list-features`` will contain
``gnulib_regex``.

See ``regex(7)`` or `the GNU Gnulib Manual
<https://www.gnu.org/software/gnulib/manual/gnulib.html#Regular-expressions>`_
for the details of the regular expression syntax.

.. note::

	The GNU regex engine supports some GNU extensions described `here
	<https://www.gnu.org/software/gnulib/manual/gnulib.html#posix_002dextended-regular-expression-syntax>`_.
	Note that an optlib parser using the extensions may not work with Universal
	Ctags on some other systems.

The POSIX Extended Regular Expressions (ERE) does
*not* support many of the "modern" extensions such as lazy captures,
non-capturing grouping, atomic grouping, possessive quantifiers, look-ahead/behind,
etc. It may be notoriously slow when backtracking.

A common error is forgetting that a
POSIX ERE engine is always *greedy*; the '``*``' and '``+``' quantifiers match
as much as possible, before backtracking from the end of their match.

For example this pattern::

	foo.*bar

Will match this entire string, not just the first part::

	foobar, bar, and even more bar

Another detail to keep in mind is how the regex engine treats newlines.
Universal Ctags compiles the regular expressions in the ``--regex-<LANG>`` and
``--mline-regex-<LANG>`` options with ``REG_NEWLINE`` set. What that means is documented
in the
`POSIX specification <https://pubs.opengroup.org/onlinepubs/9699919799/functions/regcomp.html>`_.
One obvious effect is that the regex special dot any-character '``.``' does not match
newline characters, the '``^``' anchor *does* match right after a newline, and
the '``$``' anchor matches right before a newline. A more subtle issue is this text from the
chapter "`Regular Expressions <https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap09.html>`_";
"the use of literal <newline>s or any escape sequence equivalent produces undefined
results". What that means is using a regex pattern with ``[^\n]+`` is invalid,
and indeed in glibc produces very odd results. **Never use** '``\n``' in patterns
for ``--regex-<LANG>``, and **never use them** in non-matching bracket expressions
for ``--mline-regex-<LANG>`` patterns. For the experimental ``--_mtable-regex-<LANG>``
you can safely use '``\n``' because that regex is not compiled with ``REG_NEWLINE``.

And it may also have some known "quirks"
with respect to escaping special characters in bracket expressions.
For example, a pattern of ``[^\]]+`` is invalid in POSIX ERE, because the '``]``' is
*not* special inside a bracket expression, and thus should **not** be escaped.
Most regex engines ignore this subtle detail in POSIX ERE, and instead allow
escaping it with '``\]``' inside the bracket expression and treat it as the
literal character '``]``'. GNU glibc, however, does not generate an error but
instead considers it undefined behavior, and in fact it will match very odd
things. Instead you **must** use the more unintuitive ``[^]]+`` syntax. The same
is technically true of other special characters inside a bracket expression,
such as ``[^\)]+``, which should instead be ``[^)]+``. The ``[^\)]+`` will
appear to work usually, but only because what it is really doing is matching any
character but '``\``' *or* '``)``'. The only exceptions for using '``\``' inside a
bracket expression are for '``\t``' and '``\n``', which ctags converts to their
single literal character control codes before passing the pattern to glibc.

You should always test your regex patterns against test files with strings that
do and do not match. Pay particular emphasis to when it should *not* match, and
how *much* it matches when it should.

Perl-compatible regular expressions (PCRE2) engine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Universal Ctags optionally supports `Perl-Compatible Regular Expressions (PCRE2)
<https://www.pcre.org/current/doc/html/pcre2syntax.html>`_ syntax
only if the Universal Ctags is built with ``pcre2`` library.
See the output of ``--list-features`` option to know whether your Universal
Ctags is built-with ``pcre2`` or not.

PCRE2 *does* support many "modern" extensions.
For example this pattern::

       foo.*?bar

Will match just the first part, ``foobar``, not this entire string,::

       foobar, bar, and even more bar

Regex option argument flags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many regex-based options described in this document support additional arguments
in the form of long flags. Long flags are specified with surrounding '``{``' and
'``}``'.

The general format and placement is as follows:

.. code-block:: ctags

	--regex-<LANG>=<PATTERN>/<NAME>/[<KIND>/]LONGFLAGS

Some examples:

.. code-block:: ctags

	--regex-Pod=/^=head1[ \t]+(.+)/\1/c/
	--regex-Foo=/set=[^;]+/\1/v/{icase}
	--regex-Man=/^\.TH[[:space:]]{1,}"([^"]{1,})".*/\1/t/{exclusive}{icase}{scope=push}
	--regex-Gdbinit=/^#//{exclusive}

Note that the last example only has two '``/``' forward-slashes following
the regex pattern, as a shortened form when no kind-spec exists.

The ``--mline-regex-<LANG>`` option also follows the above format. The
experimental ``--_mtable-regex-<LANG>`` option follows a slightly
modified version as well.

Regex control flags
......................................................................

.. Q: why even discuss the single-character version of the flags? Just
	make everyone use the long form.

The regex matching can be controlled by adding flags to the ``--regex-<LANG>``,
``--mline-regex-<LANG>``, and experimental ``--_mtable-regex-<LANG>`` options.
This is done by either using the single character short flags ``b``, ``e`` and
``i`` flags as explained in the *ctags.1* man page, or by using long flags
described earlier. The long flags require more typing but are much more
readable.

The mapping between the older short flag names and long flag names is:

=========== =========== ===========
short flag  long flag   description
=========== =========== ===========
b           basic       Posix basic regular expression syntax.
e           extend      Posix extended regular expression syntax (default).
i           icase       Case-insensitive matching.
=========== =========== ===========


So the following ``--regex-<LANG>`` expression:

.. code-block:: ctags

   --kinddef-m4=d,definition,definitions
   --regex-m4=/^m4_define\(\[([^]$\(]+).+$/\1/d/x

is the same as:

.. code-block:: ctags

   --kinddef-m4=d,definition,definitions
   --regex-m4=/^m4_define\(\[([^]$\(]+).+$/\1/d/{extend}

The characters '``{``' and '``}``' may not be suitable for command line
use, but long flags are mostly intended for option files.

Exclusive flag in regex
......................................................................

By default, lines read from the input files will be matched against all the
regular expressions defined with ``--regex-<LANG>``. Each successfully matched
regular expression will emit a tag.

In some cases another policy, exclusive-matching, is preferable to the
all-matching policy. Exclusive-matching means the rest of regular
expressions are not tried if one of regular expressions is matched
successfully, for that input line.

For specifying exclusive-matching the flags ``exclusive`` (long) and ``x``
(short) were introduced. For example, this is used in
:file:`optlib/gdbinit.ctags` for ignoring comment lines in gdb files,
as follows:

.. code-block:: ctags

	--regex-Gdbinit=/^#//{exclusive}

Comments in gdb files start with '``#``' so the above line is the first regex
match line in :file:`gdbinit.ctags`, so that subsequent regex matches are
not tried for the input line.

If an empty name pattern (``//``) is used for the ``--regex-<LANG>`` option,
ctags warns it as a wrong usage of the option. However, if the flags
``exclusive`` or ``x`` is specified, the warning is suppressed.
This is useful to ignore matched patterns as above.

NOTE: This flag does not make sense in the multi-line ``--mline-regex-<LANG>``
option nor the multi-table ``--_mtable-regex-<LANG>`` option.


Experimental flags
......................................................................

.. note:: These flags are experimental. They apply to all regex option
	types: basic ``--regex-<LANG>``, multi-line ``--mline-regex-<LANG>``,
	and the experimental multi-table ``--_mtable-regex-<LANG>`` option.

``_extra``

	This flag indicates the tag should only be generated if the given
	``extra`` type is enabled, as explained in ":ref:`extras`".

``_field``

	This flag allows a regex match to add additional custom fields to the
	generated tag entry, as explained in ":ref:`fields`".

``_role``

	This flag allows a regex match to generate a reference tag entry and
	specify the role of the reference, as explained in ":ref:`roles`".

.. NOT REVIEWED YET

``_anonymous=PREFIX``

	This flag allows a regex match to generate an anonymous tag entry.
	ctags gives a name starting with ``PREFIX`` and emits it.
	This flag is useful to record the position for a language object
	having no name. A lambda function in a functional programming
	language is a typical example of a language object having no name.

	Consider following input (``input.foo``):

	.. code-block:: lisp

		(let ((f (lambda (x) (+ 1 x))))
			...
			)

	Consider following optlib file (``foo.ctags``):

	.. code-block:: ctags
		:emphasize-lines: 4

		--langdef=Foo
		--map-Foo=+.foo
		--kinddef-Foo=l,lambda,lambda functions
		--regex-Foo=/.*\(lambda .*//l/{_anonymous=L}

	You can get following tags file:

	.. code-block:: console

		$ u-ctags  --options=foo.ctags -o - /tmp/input.foo
		Le4679d360100	/tmp/input.foo	/^(let ((f (lambda (x) (+ 1 x))))$/;"	l


.. _extras:

Conditional tagging with extras
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. NEEDS MORE REVIEWS

If a matched pattern should only be tagged when an ``extra`` flag is enabled,
mark the pattern with ``{_extra=XNAME}`` where ``XNAME`` is the name of the
extra. You must define a ``XNAME`` with the
``--_extradef-<LANG>=XNAME,DESCRIPTION`` option before defining a regex flag
marked ``{_extra=XNAME}``.

.. code-block:: python

	if __name__ == '__main__':
		do_something()

To capture the lines above in a python program (``input.py``), an ``extra`` flag can
be used.

.. code-block:: ctags
	:emphasize-lines: 1-2

	--_extradef-Python=main,__main__ entry points
	--regex-Python=/^if __name__ == '__main__':/__main__/f/{_extra=main}

The above optlib (``python-main.ctags``) introduces ``main`` extra to the Python parser.
The pattern matching is done only when the ``main`` is enabled.

.. code-block:: console

	$ ctags --options=python-main.ctags -o - --extras-Python='+{main}' input.py
	__main__	input.py	/^if __name__ == '__main__':$/;"	f


.. TODO: this "fields" section should probably be moved up this document, as a
	subsection in the "Regex option argument flags" section

.. _fields:

Adding custom fields to the tag output
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. NEEDS MORE REVIEWS

Exuberant Ctags allows just one of the specified groups in a regex pattern to
be used as a part of the name of a tag entry.

Universal Ctags allows using the other groups in the regex pattern.
An optlib parser can have its specific fields. The groups can be used as a
value of the fields of a tag entry.

Let's think about `Unknown`, an imaginary language.
Here is a source file (``input.unknown``) written in `Unknown`:

.. code-block:: java

	public func foo(n, m);
	protected func bar(n);
	private func baz(n,...);

With ``--regex-Unknown=...`` Exuberant Ctags can capture ``foo``, ``bar``, and ``baz``
as names. Universal Ctags can attach extra context information to the
names as values for fields. Let's focus on ``bar``. ``protected`` is a
keyword to control how widely the identifier ``bar`` can be accessed.
``(n)`` is the parameter list of ``bar``. ``protected`` and ``(n)`` are
extra context information of ``bar``.

With the following optlib file (``unknown.ctags``), ctags can attach
``protected`` to the field protection and ``(n)`` to the field signature.

.. code-block:: ctags
	:emphasize-lines: 5-9

	--langdef=unknown
	--kinddef-unknown=f,func,functions
	--map-unknown=+.unknown

	--_fielddef-unknown=protection,access scope
	--_fielddef-unknown=signature,signatures

	--regex-unknown=/^((public|protected|private) +)?func ([^\(]+)\((.*)\)/\3/f/{_field=protection:\1}{_field=signature:(\4)}
	--fields-unknown=+'{protection}{signature}'

For the line ``protected func bar(n);`` you will get following tags output::

	bar	input.unknown	/^protected func bar(n);$/;"	f	protection:protected	signature:(n)

Let's see the detail of ``unknown.ctags``.

.. code-block:: ctags

	--_fielddef-unknown=protection,access scope

``--_fielddef-<LANG>=name,description`` defines a new field for a parser
specified by *<LANG>*.  Before defining a new field for the parser,
the parser must be defined with ``--langdef=<LANG>``. ``protection`` is
the field name used in tags output. ``access scope`` is the description
used in the output of ``--list-fields`` and ``--list-fields=Unknown``.

.. code-block:: ctags

	--_fielddef-unknown=signature,signatures

This defines a field named ``signature``.

.. code-block:: ctags

	--regex-unknown=/^((public|protected|private) +)?func ([^\(]+)\((.*)\)/\3/f/{_field=protection:\1}{_field=signature:(\4)}

This option requests making a tag for the name that is specified with the group 3 of the
pattern, attaching the group 1 as a value for ``protection`` field to the tag, and attaching
the group 4 as a value for ``signature`` field to the tag. You can use the long regex flag
``_field`` for attaching fields to a tag with the following notation rule::

	{_field=FIELDNAME:GROUP}


``--fields-<LANG>=[+|-]{FIELDNAME}`` can be used to enable or disable specified field.

When defining a new parser specific field, it is disabled by default. Enable the
field explicitly to use the field. See ":ref:`Parser specific fields <parser-specific-fields>`"
about ``--fields-<LANG>`` option.

`passwd` parser is a simple example that uses ``--fields-<LANG>`` option.


.. _roles:

Capturing reference tags
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. NOT REVIEWED YET

To make a reference tag with an optlib parser, specify a role with
``_role`` long regex flag. Let's see an example:

.. code-block:: ctags
	:emphasize-lines: 3-6

	--langdef=FOO
	--kinddef-FOO=m,module,modules
	--_roledef-FOO.m=imported,imported module
	--regex-FOO=/import[ \t]+([a-z]+)/\1/m/{_role=imported}
	--extras=+r
	--fields=+r

A role must be defined before specifying it as value for ``_role`` flag.
``--_roledef-<LANG>.<KIND>=<ROLE>,<ROLEDESC>`` option is for defining a role.
See the line, ``--regex-FOO=...``.  In this parser `FOO`, the name of an
imported module is captured as a reference tag with role ``imported``.

For specifying *<KIND>* where the role is defined, you can use either a
kind letter or a kind name surrounded by '``{``' and '``}``'.

The option has two parameters separated by a comma:

*<ROLE>*

	the role name, and

*<ROLEDESC>*

	the description of the role.

The first parameter is the name of the role. The role is defined in
the kind *<KIND>* of the language *<LANG>*. In the example,
``imported`` role is defined in the ``module`` kind, which is specified
with ``m``. You can use ``{module}``, the name of the kind instead.

The kind specified in ``--_roledef-<LANG>.<KIND>`` option must be
defined *before* using the option. See the description of
``--kinddef-<LANG>`` for defining a kind.

The roles are listed with ``--list-roles=<LANG>``. The name and description
passed to ``--_roledef-<LANG>.<KIND>`` option are used in the output like::

	$ ctags --langdef=FOO --kinddef-FOO=m,module,modules \
				--_roledef-FOO.m='imported,imported module' --list-roles=FOO
	#KIND(L/N) NAME     ENABLED DESCRIPTION
	m/module   imported on      imported module


If specifying ``_role`` regex flag multiple times with different roles, you can
assign multiple roles to a reference tag.  See following input of C language

.. code-block:: C

	x  = 0;
	i += 1;

An ultra fine grained C parser may capture the variable ``x`` with
``lvalue`` role and the variable ``i`` with ``lvalue`` and ``incremented``
roles.

You can implement such roles by extending the built-in C parser:

.. code-block:: ctags
	:emphasize-lines: 2-5

	# c-extra.ctags
	--_roledef-C.v=lvalue,locator values
	--_roledef-C.v=incremented,incremented with ++ operator
	--regex-C=/([a-zA-Z_][a-zA-Z_0-9]*) *=/\1/v/{_role=lvalue}
	--regex-C=/([a-zA-Z_][a-zA-Z_0-9]*) *\+=/\1/v/{_role=lvalue}{_role=incremented}

.. code-block:: console

	$ ctags with --options=c-extra.ctags --extras=+r --fields=+r
	i	input.c	/^i += 1;$/;"	v	roles:lvalue,incremented
	x	input.c	/^x = 0;$/;"	v	roles:lvalue


Scope tracking in a regex parser
......................................................................

About the ``{scope=..}`` flag itself for scope tracking, see "FLAGS FOR
--regex-<LANG> OPTION" section of :ref:`ctags-optlib(7) <ctags-optlib(7)>`.

Example 1:

.. code-block:: python

	# in /tmp/input.foo
	class foo:
	def bar(baz):
		print(baz)
	class goo:
	def gar(gaz):
		print(gaz)

.. code-block:: ctags
	:emphasize-lines: 7,8

	# in /tmp/foo.ctags:
	--langdef=Foo
	--map-Foo=+.foo
	--kinddef-Foo=c,class,classes
	--kinddef-Foo=d,definition,definitions

	--regex-Foo=/^class[[:blank:]]+([[:alpha:]]+):/\1/c/{scope=set}
	--regex-Foo=/^[[:blank:]]+def[[:blank:]]+([[:alpha:]]+).*:/\1/d/{scope=ref}

.. code-block:: console

	$ ctags --options=/tmp/foo.ctags -o - /tmp/input.foo
	bar	/tmp/input.foo	/^    def bar(baz):$/;"	d	class:foo
	foo	/tmp/input.foo	/^class foo:$/;"	c
	gar	/tmp/input.foo	/^    def gar(gaz):$/;"	d	class:goo
	goo	/tmp/input.foo	/^class goo:$/;"	c


Example 2:

.. code-block:: c

	// in /tmp/input.pp
	class foo {
		int bar;
	}

.. code-block:: ctags
	:emphasize-lines: 7-9

	# in /tmp/pp.ctags:
	--langdef=pp
	--map-pp=+.pp
	--kinddef-pp=c,class,classes
	--kinddef-pp=v,variable,variables

	--regex-pp=/^[[:blank:]]*\}//{scope=pop}{exclusive}
	--regex-pp=/^class[[:blank:]]*([[:alnum:]]+)[[[:blank:]]]*\{/\1/c/{scope=push}
	--regex-pp=/^[[:blank:]]*int[[:blank:]]*([[:alnum:]]+)/\1/v/{scope=ref}

.. code-block:: console

	$ ctags --options=/tmp/pp.ctags -o - /tmp/input.pp
	bar	/tmp/input.pp	/^    int bar$/;"	v	class:foo
	foo	/tmp/input.pp	/^class foo {$/;"	c


Example 3:

.. code-block::

	# in /tmp/input.docdoc
	title T
	...
	section S0
	...
	section S1
	...

.. code-block:: ctags
	:emphasize-lines: 15,21

	# in /tmp/doc.ctags:
	--langdef=doc
	--map-doc=+.docdoc
	--kinddef-doc=s,section,sections
	--kinddef-doc=S,subsection,subsections

	--_tabledef-doc=main
	--_tabledef-doc=section
	--_tabledef-doc=subsection

	--_mtable-regex-doc=main/section +([^\n]+)\n/\1/s/{scope=push}{tenter=section}
	--_mtable-regex-doc=main/[^\n]+\n|[^\n]+|\n//
	--_mtable-regex-doc=main///{scope=clear}{tquit}

	--_mtable-regex-doc=section/section +([^\n]+)\n/\1/s/{scope=replace}
	--_mtable-regex-doc=section/subsection +([^\n]+)\n/\1/S/{scope=push}{tenter=subsection}
	--_mtable-regex-doc=section/[^\n]+\n|[^\n]+|\n//
	--_mtable-regex-doc=section///{scope=clear}{tquit}

	--_mtable-regex-doc=subsection/(section )//{_advanceTo=0start}{tleave}{scope=pop}
	--_mtable-regex-doc=subsection/subsection +([^\n]+)\n/\1/S/{scope=replace}
	--_mtable-regex-doc=subsection/[^\n]+\n|[^\n]+|\n//
	--_mtable-regex-doc=subsection///{scope=clear}{tquit}

.. code-block:: console

	% ctags --sort=no --fields=+nl --options=/tmp/doc.ctags -o - /tmp/input.docdoc
	SEC0	/tmp/input.docdoc	/^section SEC0$/;"	s	line:1	language:doc
	SUB0-1	/tmp/input.docdoc	/^subsection SUB0-1$/;"	S	line:3	language:doc	section:SEC0
	SUB0-2	/tmp/input.docdoc	/^subsection SUB0-2$/;"	S	line:5	language:doc	section:SEC0
	SEC1	/tmp/input.docdoc	/^section SEC1$/;"	s	line:7	language:doc
	SUB1-1	/tmp/input.docdoc	/^subsection SUB1-1$/;"	S	line:9	language:doc	section:SEC1
	SUB1-2	/tmp/input.docdoc	/^subsection SUB1-2$/;"	S	line:11	language:doc	section:SEC1


NOTE: This flag doesn't work well with ``--mline-regex-<LANG>=``.

Overriding the letter for file kind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. Q: this was fixed in https://github.com/universal-ctags/ctags/pull/331
	so can we remove this section?

One of the built-in tag kinds in Universal Ctags is the ``F`` file kind.
Overriding the letter for file kind is not allowed in Universal Ctags.

.. warning::

	Don't use ``F`` as a kind letter in your parser. (See issue `#317
	<https://github.com/universal-ctags/ctags/issues/317>`_ on github)

Generating fully qualified tags automatically from scope information
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If scope fields are filled properly with ``{scope=...}`` regex flags,
you can use the field values for generating fully qualified tags.
About the ``{scope=..}`` flag itself, see "FLAGS FOR --regex-<LANG>
OPTION" section of :ref:`ctags-optlib(7) <ctags-optlib(7)>`.

Specify ``{_autoFQTag}`` to the end of ``--langdef=<LANG>`` option like
``--langdef=Foo{_autoFQTag}`` to make ctags generate fully qualified
tags automatically.

'``.``' is the (ctags global) default separator combining names into a
fully qualified tag. You can customize separators with
``--_scopesep-<LANG>=...`` option.

input.foo::

  class X
     var y
  end

foo.ctags:

.. code-block:: ctags
	:emphasize-lines: 1

	--langdef=foo{_autoFQTag}
	--map-foo=+.foo
	--kinddef-foo=c,class,classes
	--kinddef-foo=v,var,variables
	--regex-foo=/class ([A-Z]*)/\1/c/{scope=push}
	--regex-foo=/end///{placeholder}{scope=pop}
	--regex-foo=/[ \t]*var ([a-z]*)/\1/v/{scope=ref}

Output::

	$ u-ctags --quiet --options=./foo.ctags -o - input.foo
	X	input.foo	/^class X$/;"	c
	y	input.foo	/^	var y$/;"	v	class:X

	$ u-ctags --quiet --options=./foo.ctags --extras=+q -o - input.foo
	X	input.foo	/^class X$/;"	c
	X.y	input.foo	/^	var y$/;"	v	class:X
	y	input.foo	/^	var y$/;"	v	class:X


``X.y`` is printed as a fully qualified tag when ``--extras=+q`` is given.

.. NOT REVIEWED YET (--_scopesep)

Customizing scope separators
......................................................................
Use ``--_scopesep-<LANG>=[<parent-kindLetter>]/<child-kindLetter>:<sep>``
option for customizing if the language uses ``{_autoFQTag}``.

``parent-kindLetter``

	The kind letter for a tag of outer-scope.

	You can use '``*``' for specifying as wildcards that means
	*any kinds* for a tag of outer-scope.

	If you omit ``parent-kindLetter``, the separator is used as
	a prefix for tags having the kind specified with ``child-kindLetter``.
	This prefix can be used to refer to global namespace or similar concepts if the
	language has one.

``child-kindLetter``

	The kind letter for a tag of inner-scope.

	You can use '``*``' for specifying as wildcards that means
	*any kinds* for a tag of inner-scope.

``sep``

	In a qualified tag, if the outer-scope has kind and ``parent-kindLetter``
	the inner-scope has ``child-kindLetter``, then ``sep`` is instead in
	between the scope names in the generated tags file.

specifying '``*``' as both  ``parent-kindLetter`` and ``child-kindLetter``
sets ``sep`` as the language default separator. It is used as fallback.

Specifying '``*``' as ``child-kindLetter`` and omitting ``parent-kindLetter``
sets ``sep`` as the language default prefix. It is used as fallback.


NOTE: There is no ctags global default prefix.

NOTE: ``_scopesep-<LANG>=...`` option affects only a parser that
enables ``_autoFQTag``. A parser building full qualified tags
manually ignores the option.

Let's see an example.
The input file is written in Tcl.  Tcl parser is not an optlib
parser. However, it uses the ``_autoFQTag`` feature internally.
Therefore, ``_scopesep-Tcl=`` option works well. Tcl parser
defines two kinds ``n`` (``namespace``) and ``p`` (``procedure``).

By default, Tcl parser uses ``::`` as scope separator. The parser also
uses ``::`` as root prefix.

.. code-block:: tcl

	namespace eval N {
		namespace eval M {
			proc pr0 {s} {
				puts $s
			}
		}
	}

	proc pr1 {s} {
		puts $s
	}

``M`` is defined under the scope of ``N``. ``pr0`` is defined	under the scope
of ``M``. ``N`` and ``pr1`` are at top level (so they are candidates to be added
prefixes). ``M`` and ``N`` are language objects with ``n`` (``namespace``) kind.
``pr0`` and ``pr1`` are language objects with ``p`` (``procedure``) kind.

.. code-block:: console

	$ ctags -o - --extras=+q input.tcl
	::N	input.tcl	/^namespace eval N {$/;"	n
	::N::M	input.tcl	/^	namespace eval M {$/;"	n	namespace:::N
	::N::M::pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:::N::M
	::pr1	input.tcl	/^proc pr1 {s} {$/;"	p
	M	input.tcl	/^	namespace eval M {$/;"	n	namespace:::N
	N	input.tcl	/^namespace eval N {$/;"	n
	pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:::N::M
	pr1	input.tcl	/^proc pr1 {s} {$/;"	p

Let's change the default separator to ``->``:

.. code-block:: console
	:emphasize-lines: 1

	$ ctags -o - --extras=+q --_scopesep-Tcl='*/*:->' input.tcl
	::N	input.tcl	/^namespace eval N {$/;"	n
	::N->M	input.tcl	/^	namespace eval M {$/;"	n	namespace:::N
	::N->M->pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:::N->M
	::pr1	input.tcl	/^proc pr1 {s} {$/;"	p
	M	input.tcl	/^	namespace eval M {$/;"	n	namespace:::N
	N	input.tcl	/^namespace eval N {$/;"	n
	pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:::N->M
	pr1	input.tcl	/^proc pr1 {s} {$/;"	p

Let's define '``^``' as default prefix:

.. code-block:: console
	:emphasize-lines: 1

	$ ctags -o - --extras=+q --_scopesep-Tcl='*/*:->' --_scopesep-Tcl='/*:^' input.tcl
	M	input.tcl	/^	namespace eval M {$/;"	n	namespace:^N
	N	input.tcl	/^namespace eval N {$/;"	n
	^N	input.tcl	/^namespace eval N {$/;"	n
	^N->M	input.tcl	/^	namespace eval M {$/;"	n	namespace:^N
	^N->M->pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:^N->M
	^pr1	input.tcl	/^proc pr1 {s} {$/;"	p
	pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:^N->M
	pr1	input.tcl	/^proc pr1 {s} {$/;"	p

Let's override the specification of separator for combining a
namespace and a procedure with '``+``': (About the separator for
combining a namespace and another namespace, ctags uses the default separator.)

.. code-block:: console
	:emphasize-lines: 1

	$ ctags -o - --extras=+q --_scopesep-Tcl='*/*:->' --_scopesep-Tcl='/*:^' --_scopesep-Tcl='n/p:+' input.tcl
	M	input.tcl	/^	namespace eval M {$/;"	n	namespace:^N
	N	input.tcl	/^namespace eval N {$/;"	n
	^N	input.tcl	/^namespace eval N {$/;"	n
	^N->M	input.tcl	/^	namespace eval M {$/;"	n	namespace:^N
	^N->M+pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:^N->M
	^pr1	input.tcl	/^proc pr1 {s} {$/;"	p
	pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:^N->M
	pr1	input.tcl	/^proc pr1 {s} {$/;"	p

Let's override the definition of prefix for a namespace with '``@``':
(About the prefix for procedures, ctags uses the default prefix.)

.. code-block:: console
	:emphasize-lines: 1

	$ ctags -o - --extras=+q --_scopesep-Tcl='*/*:->' --_scopesep-Tcl='/*:^' --_scopesep-Tcl='n/p:+' --_scopesep-Tcl='/n:@' input.tcl
	@N	input.tcl	/^namespace eval N {$/;"	n
	@N->M	input.tcl	/^	namespace eval M {$/;"	n	namespace:@N
	@N->M+pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:@N->M
	M	input.tcl	/^	namespace eval M {$/;"	n	namespace:@N
	N	input.tcl	/^namespace eval N {$/;"	n
	^pr1	input.tcl	/^proc pr1 {s} {$/;"	p
	pr0	input.tcl	/^		proc pr0 {s} {$/;"	p	namespace:@N->M
	pr1	input.tcl	/^proc pr1 {s} {$/;"	p


Multi-line pattern match
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We often need to scan multiple lines to generate a tag, whether due to
needing contextual information to decide whether to tag or not, or to
constrain generating tags to only certain cases, or to grab multiple
substrings to generate the tag name.

Universal Ctags has two ways to accomplish this: *multi-line regex options*,
and an experimental *multi-table regex options* described later.

The newly introduced ``--mline-regex-<LANG>`` is similar to ``--regex-<LANG>``
except the pattern is applied to the whole file's contents, not line by line.

This example is based on an issue `#219
<https://github.com/universal-ctags/ctags/issues/219>`_ posted by
@andreicristianpetcu:

.. code-block:: java

	// in input.java:

	@Subscribe
	public void catchEvent(SomeEvent e)
	{
	   return;
	}

	@Subscribe
	public void
	recover(Exception e)
	{
	    return;
	}

The above java code is similar to the Java `Spring <https://spring.io>`_
framework. The ``@Subscribe`` annotation is a keyword for the framework, and the
developer would like to have a tag generated for each method annotated with
``@Subscribe``, using the name of the method followed by a dash followed by the
type of the argument. For example the developer wants the tag name
``Event-SomeEvent`` generated for the first method shown above.

To accomplish this, the developer creates a :file:`spring.ctags` file with
the following:

.. code-block:: ctags
	:emphasize-lines: 4

	# in spring.ctags:
	--langdef=javaspring
	--map-javaspring=+.java
	--mline-regex-javaspring=/@Subscribe([[:space:]])*([a-z ]+)[[:space:]]*([a-zA-Z]*)\(([a-zA-Z]*)/\3-\4/s,subscription/{mgroup=3}
	--fields=+ln

And now using :file:`spring.ctags` the tag file has this:

.. code-block:: console

	$ ctags -o - --options=./spring.ctags input.java
	Event-SomeEvent	input.java	/^public void catchEvent(SomeEvent e)$/;"	s	line:2	language:javaspring
	recover-Exception	input.java	/^    recover(Exception e)$/;"	s	line:10	language:javaspring

Multiline pattern flags
......................................................................

.. note:: These flags also apply to the experimental ``--_mtable-regex-<LANG>``
	option described later.

``{mgroup=N}``

	This flag indicates the pattern should be applied to the whole file
	contents, not line by line. ``N`` is the number of a capture group in the
	pattern, which is used to record the line number location of the tag. In the
	above example ``3`` is specified. The start position of the regex capture
	group 3, relative to the whole file is used.

.. warning:: You **must** add an ``{mgroup=N}`` flag to the multi-line
	``--mline-regex-<LANG>`` option, even if the ``N`` is ``0`` (meaning the
	start position of the whole regex pattern). You do not need to add it for
	the multi-table ``--_mtable-regex-<LANG>``.

.. TODO: Q: isn't the above restriction really a bug? I think it is. I should fix it.
   Q to @masatake-san: Do you mean that {mgroup=0} can be omitted? -> #2918 is opened


``{_advanceTo=N[start|end]}``

	A regex pattern is applied to whole file's contents iteratively. This long
	flag specifies from where the pattern should be applied in the next
	iteration for regex matching. When a pattern matches, the next pattern
	matching starts from the start or end of capture group ``N``. By default it
	advances to the end of the whole match (i.e., ``{_advanceTo=0end}`` is
	the default).


	Let's think about following input
	::

	   def def abc

	Consider two sets of options, ``foo.ctags`` and ``bar.ctags``.

	.. code-block:: ctags
		:emphasize-lines: 5

		# foo.ctags:
	   	--langdef=foo
	   	--langmap=foo:.foo
	   	--kinddef-foo=a,something,something
	   	--mline-regex-foo=/def *([a-z]+)/\1/a/{mgroup=1}


	.. code-block:: ctags
		:emphasize-lines: 5

		# bar.ctags:
		--langdef=bar
		--langmap=bar:.bar
		--kinddef-bar=a,something,something
		--mline-regex-bar=/def *([a-z]+)/\1/a/{mgroup=1}{_advanceTo=1start}

	``foo.ctags`` emits following tags output::

	   def	input.foo	/^def def abc$/;"	a

	``bar.ctags`` emits following tags output::

	   def	input-0.bar	/^def def abc$/;"	a
	   abc	input-0.bar	/^def def abc$/;"	a

	``_advanceTo=1start`` is specified in ``bar.ctags``.
	This allows ctags to capture ``abc``.

	At the first iteration, the patterns of both
	``foo.ctags`` and ``bar.ctags`` match as follows
	::

		0   1       (start)
		v   v
		def def abc
		       ^
		       0,1  (end)

	``def`` at the group 1 is captured as a tag in
	both languages. At the next iteration, the positions
	where the pattern matching is applied to are not the
	same in the languages.

	``foo.ctags``
	::

		       0end (default)
		       v
		def def abc


	``bar.ctags``
	::

		    1start (as specified in _advanceTo long flag)
		    v
		def def abc

	This difference of positions makes the difference of tags output.

	A more relevant use-case is when ``{_advanceTo=N[start|end]}`` is used in
	the experimental ``--_mtable-regex-<LANG>``, to "advance" back to the
	beginning of a match, so that one can generate multiple tags for the same
	input line(s).

.. note:: This flag doesn't work well with scope related flags and ``exclusive`` flags.


.. Q: this was previously titled "Byte oriented pattern matching...", presumably
	because it "matched against the input at the current byte position, not line".
	But that's also true for --mline-regex-<LANG>, as far as I can tell.

Advanced pattern matching with multiple regex tables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note:: This is a highly experimental feature. This will not go into
	the man page of 6.0. But let's be honest, it's the most exciting feature!

In some cases, the ``--regex-<LANG>`` and ``--mline-regex-<LANG>`` options are not
sufficient to generate the tags for a particular language. Some of the common
reasons for this are:

* To ignore commented lines or sections for the language file, so that
  tags aren't generated for symbols that are within the comments.
* To enter and exit scope, and use it for tagging based on contextual
  state or with end-scope markers that are difficult to match to their
  associated scope entry point.
* To support nested scopes.
* To change the pattern searched for, or the resultant tag for the same
  pattern, based on scoping or contextual location.
* To break up an overly complicated ``--mline-regex-<LANG>`` pattern into
  separate regex patterns, for performance or readability reasons.

To help handle such things, Universal Ctags has been enhanced with multi-table
regex matching. The feature is inspired by `lex`, the fast lexical analyzer
generator, which is a popular tool on Unix environments for writing parsers, and
`RegexLexer <http://pygments.org/docs/lexerdevelopment/>`_ of Pygments.
Knowledge about them will help you understand the new options.

The new options are:

``--_tabledef-<LANG>``
	Declares a new regex matching table of a given name for the language,
	as described in ":ref:`tabledef`".

``--_mtable-regex-<LANG>``
	Adds a regex pattern and associated tag generation information and flags, to
	the given table, as described in ":ref:`mtable_regex`".

``--_mtable-extend-<LANG>``
	Includes a previously-defined regex table to the named one.

The above will be discussed in more detail shortly.

First, let's explain the feature with an example. Consider an
imaginary language `X` has a similar syntax as JavaScript: ``var`` is
used as defining variable(s), and "``/* ... */``" is used for block
comments.

Here is our input, :file:`input.x`:

.. code-block:: java

   /* BLOCK COMMENT
   var dont_capture_me;
   */
   var a /* ANOTHER BLOCK COMMENT */, b;

We want ctags to capture ``a`` and ``b`` - but it is difficult to write a parser
that will ignore ``dont_capture_me`` in the comment with a classical regex
parser defined with ``--regex-<LANG>`` or ``--mline-regex-<LANG>``, because of
the block comments.

The ``--regex-<LANG>`` option only works on one line at a time, so can not know
``dont_capture_me`` is within comments. The ``--mline-regex-<LANG>`` could
do it in theory, but due to the greedy nature of the regex engine it is
impractical and potentially inefficient to do so, given that there could be
multiple block comments in the file, with '``*``' inside them, etc.

A parser written with multi-table regex, on the other hand, can capture only
``a`` and ``b`` safely. But it is more complicated to understand.

Here is the 1st version of :file:`X.ctags`:

.. code-block:: ctags

   --langdef=X
   --map-X=.x
   --kinddef-X=v,var,variables

Not so interesting. It doesn't really *do* anything yet. It just creates a new
language named ``X``, for files ending with a :file:`.x` suffix, and defines a
new tag for variable kinds.

When writing a multi-table parser, you have to think about the necessary states
of parsing. For the parser of language `X`, we need the following states:

* `toplevel` (initial state)
* `comment` (inside comment)
* `vars` (var statements)

.. _tabledef:

Declaring a new regex table
......................................................................

Before adding regular expressions, you have to declare tables for each state
with the ``--_tabledef-<LANG>=<TABLE>`` option.

Here is the 2nd version of :file:`X.ctags` doing so:

.. code-block:: ctags
	:emphasize-lines: 5-7

	--langdef=X
	--map-X=.x
	--kinddef-X=v,var,variables

	--_tabledef-X=toplevel
	--_tabledef-X=comment
	--_tabledef-X=vars

For table names, only characters in the range ``[0-9a-zA-Z_]`` are acceptable.

For a given language, for each file's input the ctags multi-table parser begins
with the first declared table. For :file:`X.ctags`, ``toplevel`` is the one.
The other tables are only ever entered/checked if another table specified to do
so, starting with the first table. In other words, if the first declared table
does not find a match for the current input, and does not specify to go to
another table, the other tables for that language won't be used. The flags to go
to another table are ``{tenter}``, ``{tleave}``, and ``{tjump}``, as described
later.

.. _mtable_regex:

Adding a regex to a regex table
......................................................................

The new option to add a regex to a declared table is ``--_mtable-regex-<LANG>``,
and it follows this form:

.. code-block:: ctags

	--_mtable-regex-<LANG>=<TABLE>/<PATTERN>/<NAME>/[<KIND>]/LONGFLAGS

The parameters for ``--_mtable-regex-<LANG>`` look complicated. However,
``<PATTERN>``, ``<NAME>``, and ``<KIND>`` are the same as the parameters of the
``--regex-<LANG>`` and ``--mline-regex-<LANG>`` options. ``<TABLE>`` is simply
the name of a table previously declared with the ``--_tabledef-<LANG>`` option.

A regex pattern added to a parser with ``--_mtable-regex-<LANG>`` is matched
against the input at the current byte position, not line. Even if you do not
specify the '``^``' anchor at the start of the pattern, ctags adds '``^``' to
the pattern automatically. Unlike the ``--regex-<LANG>`` and
``--mline-regex-<LANG>`` options, a '``^``' anchor does not mean "beginning of
line" in ``--_mtable-regex-<LANG>``; instead it means the beginning of the
input string (i.e., the current byte position).

The ``LONGFLAGS`` include the already discussed flags for ``--regex-<LANG>`` and
``--mline-regex-<LANG>``: ``{scope=...}``, ``{mgroup=N}``, ``{_advanceTo=N}``,
``{basic}``, ``{extend}``, and ``{icase}``. The ``{exclusive}`` flag does not
make sense for multi-table regex.

In addition, several new flags are introduced exclusively for multi-table
regex use:

``{tenter}``
	Push the current table on the stack, and enter another table.

``{tleave}``
	Leave the current table, pop the stack, and go to the table that was
	just popped from the stack.

``{tjump}``
	Jump to another table, without affecting the stack.

``{treset}``
	Clear the stack, and go to another table.

``{tquit}``
	Clear the stack, and stop processing the current input file for this
	language.

To explain the above new flags, we'll continue using our example in the
next section.

Skipping block comments
......................................................................

Let's continue with our example. Here is the 3rd version of :file:`X.ctags`:

.. code-block:: ctags
	:emphasize-lines: 9-13
	:linenos:

	--langdef=X
	--map-X=.x
	--kinddef-X=v,var,variables

	--_tabledef-X=toplevel
	--_tabledef-X=comment
	--_tabledef-X=vars

	--_mtable-regex-X=toplevel/\/\*//{tenter=comment}
	--_mtable-regex-X=toplevel/.//

	--_mtable-regex-X=comment/\*\///{tleave}
	--_mtable-regex-X=comment/.//

Four ``--_mtable-regex-X`` lines are added for skipping the block comments. Let's
discuss them one by one.

For each new file it scans, ctags always chooses the first pattern of the
first table of the parser. Even if it's an empty table, ctags will only try
the first declared table. (in such a case it would immediately fail to match
anything, and thus stop processing the input file and effectively do nothing)

The first declared table (``toplevel``) has the following regex added to
it first:

.. code-block:: ctags
	:linenos:
	:lineno-start: 9

	--_mtable-regex-X=toplevel/\/\*//{tenter=comment}

A pattern of ``\/\*`` is added to the ``toplevel`` table, to match the
beginning of a block comment. A backslash character is used in front of the
leading '``/``' to escape the separation character '``/``' that separates the fields
of ``--_mtable-regex-<LANG>``. Another backslash inside the pattern is used
before the asterisk '``*``', to make it a literal asterisk character in regex.

The last ``//`` means ctags should not tag something matching this pattern.
In ``--regex-<LANG>`` you never use ``//`` because it would be pointless to
match something and not tag it using and single-line ``--regex-<LANG>``; in
multi-line ``--mline-regex-<LANG>`` you rarely see it, because it would rarely
be useful. But in multi-table regex it's quite common, since you frequently
want to transition from one state to another (i.e., ``tenter`` or ``tjump``
from one table to another).

The long flag added to our first regex of our first table is ``tenter``, which
is a long flag for switching the table and pushing on the stack. ``{tenter=comment}``
means "switch the table from toplevel to comment".

So given the input file :file:`input.x` shown earlier, ctags will begin at
the ``toplevel`` table and try to match the first regex. It will succeed, and
thus push on the stack and go to the ``comment`` table.

It will begin at the top of the ``comment`` table (it always begins at the top
of a given table), and try each regex line in sequence until it finds a match.
If it fails to find a match, it will pop the stack and go to the table that was
just popped from the stack, and begin trying to match at the top of *that* table.
If it continues failing to find a match, and ultimately reaches the end of the
stack, it will stop processing for this file. For the next input file, it will
begin again from the top of the first declared table.

Getting back to our example, the top of the ``comment`` table has this regex:

.. code-block:: ctags
	:linenos:
	:lineno-start: 12

	--_mtable-regex-X=comment/\*\///{tleave}

Similar to the previous ``toplevel`` table pattern, this one for ``\*\/`` uses
a backslash to escape the separator '``/``', as well as one before the '``*``' to
make it a literal asterisk in regex. So what it's looking for, from a simple
string perspective, is the sequence ``*/``. Note that this means even though
you see three backslashes ``///`` at the end, the first one is escaped and used
for the pattern itself, and the ``--_mtable-regex-X`` only has ``//`` to
separate the regex pattern from the long flags, instead of the usual ``///``.
Thus it's using the shorthand form of the ``--_mtable-regex-X`` option.
It could instead have been:

.. code-block:: ctags

	--_mtable-regex-X=comment/\*\////{tleave}

The above would have worked exactly the same.

Getting back to our example, remember we're looking at the :file:`input.x`
file, currently using the ``comment`` table, and trying to match the first
regex of that table, shown above, at the following location::

	   ,ctags is trying to match starting here
	  v
	/* BLOCK COMMENT
	var dont_capture_me;
	*/
	var a /* ANOTHER BLOCK COMMENT */, b;

The pattern doesn't match for the position just after ``/*``, because that
position is a space character. So ctags tries the next pattern in the same
table:

.. code-block:: ctags
	:linenos:
	:lineno-start: 13

	--_mtable-regex-X=comment/.//

This pattern matches any any one character including newline; the current
position moves one character forward. Now the character at the current position is
'``B``'. The first pattern of the table ``*/`` still does not match with the input. So
ctags uses next pattern again. When the current position moves to the ``*/``
of the 3rd line of :file:`input.x`, it will finally match this:

.. code-block:: ctags
	:linenos:
	:lineno-start: 12

	--_mtable-regex-X=comment/\*\///{tleave}

In this pattern, the long flag ``{tleave}`` is specified. This triggers table
switching again. ``{tleave}`` makes ctags switch the table back to the last
table used before doing ``{tenter}``. In this case, ``toplevel`` is the table.
ctags manages a stack where references to tables are put. ``{tenter}`` pushes
the current table to the stack. ``{tleave}`` pops the table at the top of the
stack and chooses it.

So now ctags is back to the ``toplevel`` table, and tries the first regex
of that table, which was this:

.. code-block:: ctags
	:linenos:
	:lineno-start: 9

	--_mtable-regex-X=toplevel/\/\*//{tenter=comment}

It tries to match that against its current position, which is now the
newline on line 3, between the ``*/`` and the word ``var``::

	/* BLOCK COMMENT
	var dont_capture_me;
	*/ <--- ctags is now at this newline (/n) character
	var a /* ANOTHER BLOCK COMMENT */, b;

The first regex of the ``toplevel`` table does not match a newline, so it tries
the second regex:

.. code-block:: ctags
	:linenos:
	:lineno-start: 13

	--_mtable-regex-X=toplevel/.//

This matches a newline successfully, but has no actions to perform. So ctags
moves one character forward (the newline it just matched), and goes back to the
top of the ``toplevel`` table, and tries the first regex again. Eventually we'll
reach the beginning of the second block comment, and do the same things as before.

When ctags finally reaches the end of the file (the position after ``b;``),
it will not be able to match either the first or second regex of the
``toplevel`` table, and quit processing the input file.

So far, we've successfully skipped over block comments for our new ``X``
language, but haven't generated any tags. The point of ctags is to generate
tags, not just keep your computer warm. So now let's move onto actually tagging
variables...


Capturing variables in a sequence
......................................................................

Here is the 4th version of :file:`X.ctags`:

.. code-block:: ctags
	:emphasize-lines: 10,16-19
	:linenos:

	--langdef=X
	--map-X=.x
	--kinddef-X=v,var,variables

	--_tabledef-X=toplevel
	--_tabledef-X=comment
	--_tabledef-X=vars

	--_mtable-regex-X=toplevel/\/\*//{tenter=comment}
	--_mtable-regex-X=toplevel/var[ \n\t]//{tenter=vars}
	--_mtable-regex-X=toplevel/.//

	--_mtable-regex-X=comment/\*\///{tleave}
	--_mtable-regex-X=comment/.//

	--_mtable-regex-X=vars/;//{tleave}
	--_mtable-regex-X=vars/\/\*//{tenter=comment}
	--_mtable-regex-X=vars/([a-zA-Z][a-zA-Z0-9]*)/\1/v/
	--_mtable-regex-X=vars/.//

One pattern in ``toplevel`` was added, and a new table ``vars`` with four
patterns was also added.

The new regex in ``toplevel`` is this:

.. code-block:: ctags
	:linenos:
	:lineno-start: 10

	--_mtable-regex-X=toplevel/var[ \n\t]//{tenter=vars}

The purpose of this being in `toplevel` is to switch to the `vars` table when
the keyword ``var`` is found in the input stream. We need to switch states
(i.e., tables) because we can't simply capture the variables ``a`` and ``b``
with a single regex pattern in the ``toplevel`` table, because there might be
block comments inside the ``var`` statement (as there are in our
:file:`input.x`), and we also need to create *two* tags: one for ``a`` and one
for ``b``, even though the word ``var`` only appears once. In other words, we
need to "remember" that we saw the keyword ``var``, when we later encounter the
names ``a`` and ``b``, so that we know to tag each of them; and saving that
"in-variable-statement" state is accomplished by switching tables to the
``vars`` table.

The first regex in our new ``vars`` table is:

.. code-block:: ctags
	:linenos:
	:lineno-start: 16

	--_mtable-regex-X=vars/;//{tleave}

This pattern is used to match a single semi-colon '``;``', and if it matches
pop back to the ``toplevel`` table using the ``{tleave}`` long flag. We
didn't have to make this the first regex pattern, because it doesn't overlap
with any of the other ones other than the ``/.//`` last one (which must be
last for this example to work).

The second regex in our ``vars`` table is:

.. code-block:: ctags
	:linenos:
	:lineno-start: 17

	--_mtable-regex-X=vars/\/\*//{tenter=comment}

We need this because block comments can be in variable definitions::

   var a /* ANOTHER BLOCK COMMENT */, b;

So to skip block comments in such a position, the pattern ``\/\*`` is used just
like it was used in the ``toplevel`` table: to find the literal ``/*`` beginning
of the block comment and enter the ``comment`` table. Because we're using
``{tenter}`` and ``{tleave}`` to push/pop from a stack of tables, we can
use the same ``comment`` table for both ``toplevel`` and ``vars`` to go to,
because ctags will *remember* the previous table and ``{tleave}`` will
pop back to the right one.

The third regex in our ``vars`` table is:

.. code-block:: ctags
	:linenos:
	:lineno-start: 18

	--_mtable-regex-X=vars/([a-zA-Z][a-zA-Z0-9]*)/\1/v/

This is nothing special, but is the one that actually tags something: it
captures the variable name and uses it for generating a ``variable`` (shorthand
``v``) tag kind.

The last regex in the ``vars`` table we've seen before:

.. code-block:: ctags
	:linenos:
	:lineno-start: 19

	--_mtable-regex-X=vars/.//

This makes ctags ignore any other characters, such as whitespace or the
comma '``,``'.


Running our example
......................................................................

.. code-block:: console

	$ cat input.x
	/* BLOCK COMMENT
	var dont_capture_me;
	*/
	var a /* ANOTHER BLOCK COMMENT */, b;

	$ u-ctags -o - --fields=+n --options=X.ctags input.x
	u-ctags -o - --fields=+n --options=X.ctags input.x
	a	input.x	/^var a \/* ANOTHER BLOCK COMMENT *\/, b;$/;"	v	line:4
	b	input.x	/^var a \/* ANOTHER BLOCK COMMENT *\/, b;$/;"	v	line:4

It works!

You can find additional examples of multi-table regex in our github repo, under
the ``optlib`` directory. For example ``puppetManifest.ctags`` is a serious
example. It is the primary parser for testing multi-table regex parsers, and
used in the actual ctags program for parsing puppet manifest files.


.. _guest-regex-flag:

Scheduling a guest parser with ``_guest`` regex flag
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. NOT REVIEWED YET

With ``_guest`` regex flag, you can run a parser (a guest parser) on an
area of the current input file.
See ":ref:`host-guest-parsers`" about the concept of the guest parser.

The ``_guest`` regex flag specifies a *guest spec*, and attaches it to
the associated regex pattern.

A guest spec has three fields: *<PARSER>*, *<START>* of area, and *<END>* of area.
The ``_guest`` regex flag has following forms::

  {_guest=<PARSER>,<START>,<END>}

ctags maintains a data called *guest request* during parsing.  A
guest request also has three fields: `parser`, `start of area`, and
`end of area`.

You, a parser developer, have to fill the fields of guest specs.
ctags inquiries the guest spec when matching the regex pattern
associated with it, tries to fill the fields of the guest request,
and runs a guest parser when all the fields of the guest request are
filled.

If you use `Multi-line pattern match`_ to define a host parser,
you must specify all the fields of `guest request`.

On the other hand if you don't use `Multi-line pattern match`_ to define a host parser,
ctags can fill fields of `guest request` incrementally; more than
one guest specs are used to fill the fields. In other words, you can
make some of the fields of a guest spec empty.

The *<PARSER>* field of ``_guest`` regex flag
......................................................................
For *<PARSER>*, you can specify one of the following items:

a name of a parser

	If you know the guest parser you want to run before parsing
	the input file, specify the name of the parser. Aliases of parsers
	are also considered when finding a parser for the name.

	An example of running C parser as a guest parser::

		{_guest=C,...

the group number of a regex pattern started from '``\``' (backslash)

	If a parser name appears in an input file, write a regex pattern
	to capture the name.  Specify the group number where the name is
	stored to the parser.  In such case, use '``\``' as the prefix for
	the number. Aliases of parsers are also considered when finding
	a parser for the name.

	Let's see an example. Git Flavor Markdown (GFM) is a language for
	documentation. It provides a notation for quoting a snippet of
	program code; the language treats the area started from ``~~~`` to
	``~~~`` as a snippet. You can specify a programming language of
	the snippet with starting the area with
	``~~~<THE_NAME_OF_LANGUAGE>``, like ``~~~C`` or ``~~~Java``.

	To run a guest parser on the area, you have to capture the
	*<THE_NAME_OF_LANGUAGE>* with a regex pattern:

	.. code-block:: ctags

		--_mtable-regex-Markdown=main/~~~([a-zA-Z0-9][-#+a-zA-Z0-9]*)[\n]//{_guest=\1,0end,}

	The pattern captures the language name in the input file with the
	regex group 1, and specify it to *<PARSER>*::

		{guest=\1,...

the group number of a regex pattern started from '``*``' (asterisk)

	If a file name implying a programming language appears in an input
	file, capture the file name with the regex pattern where the guest
	spec attaches to. ctags tries to find a proper parser for the
	file name by inquiring the langmap.

	Use '``*``' as the prefix to the number for specifying the group of
	the regex pattern that captures the file name.

	Let's see an example. Consider you have a shell script that emits
	a program code instantiated from one of the templates. Here documents
	are used to represent the templates like:

	.. code-block:: sh

		i=...
		cat > foo.c <<EOF
			int main (void) { return $i; }
		EOF

		cat > foo.el <<EOF
			(defun foo () (1+ $i))
		EOF

	To run guest parsers for the here document areas, the shell
	script parser of ctags must choose the parsers from the file
	names (``foo.c`` and ``foo.el``):

	.. code-block:: ctags

		--regex-sh=/cat > ([a-z.]+) <<EOF//{_guest=*1,0end,}

	The pattern captures the file name in the input file with the
	regex group 1, and specify it to *<PARSER>*::

	   {_guest=*1,...

The *<START>* and *<END>* fields of `_guest` regex flag
......................................................................

The *<START>* and *<END>* fields specify the area the *<PARSER>* parses.  *<START>*
specifies the start of the area. *<END>* specifies the end of the area.

The forms of the two fields are the same: a regex group number
followed by ``start`` or ``end``. e.g. ``3start``, ``0end``.  The suffixes,
``start`` and ``end``, represents one of two boundaries of the group.

Let's see an example::

	{_guest=C,2end,3start}

This guest regex flag means running C parser on the area between
``2end`` and ``3start``. ``2end`` means the area starts from the end of
matching of the 2nd regex group associated with the flag. ``3start``
means the area ends at the beginning of matching of the 3rd regex
group associated with the flag.

Let's more realistic example.
Here is an optlib file for an imaginary language `single`:

.. code-block:: ctags
	:emphasize-lines: 3

	--langdef=single
	--map-single=.single
	--regex-single=/^(BEGIN_C<).*(>END_C)$//{_guest=C,1end,2start}

This parser can run C parser and extract ``main`` function from the
following input file::

	BEGIN_C<int main (int argc, char **argv) { return 0; }>END_C
	        ^                                             ^
	         `- "1end" points here.                       |
	                               "2start" points here. -+

.. NOT REVIEWED YET

.. _defining-subparsers:

Defining a subparser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Basic
.........................................................................

About the concept of subparser, see ":ref:`base-sub-parsers`".

``--langdef=<LANG>`` option is extended as
``--langdef=<LANG>[{base=<LANG>}[{shared|dedicated|bidirectional}]][{_autoFQTag}]`` to define
a subparser for a specified base parser. Combining with ``--kinddef-<LANG>``
and ``--regex-<KIND>`` options, you can extend an existing parser
without risk of kind confliction.

Let's see an example.

input.c

.. code-block:: C

    static int set_one_prio(struct task_struct *p, int niceval, int error)
    {
    }

    SYSCALL_DEFINE3(setpriority, int, which, int, who, int, niceval)
    {
	    ...;
    }

.. code-block:: console

    $ ctags  -x --_xformat="%20N %10K %10l"  -o - input.c
	    set_one_prio   function          C
	 SYSCALL_DEFINE3   function          C

C parser doesn't understand that ``SYSCALL_DEFINE3`` is a macro for defining an
entry point for a system.

Let's define `linux` subparser which using C parser as a base parser (``linux.ctags``):

.. code-block:: ctags
	:emphasize-lines: 1,3

	--langdef=linux{base=C}
	--kinddef-linux=s,syscall,system calls
	--regex-linux=/SYSCALL_DEFINE[0-9]\(([^, )]+)[\),]*/\1/s/

The output is change as follows with `linux` parser:

.. code-block:: console
	:emphasize-lines: 2

	$ ctags --options=./linux.ctags -x --_xformat="%20N %10K %10l"  -o - input.c
		 setpriority    syscall      linux
		set_one_prio   function          C
	     SYSCALL_DEFINE3   function          C

``setpriority`` is recognized as a ``syscall`` of `linux`.

Using only ``--regex-C=...`` you can capture ``setpriority``.
However, there were concerns about kind confliction; when introducing
a new kind with ``--regex-C=...``, you cannot use a letter and name already
used in C parser and ``--regex-C=...`` options specified in the other places.

You can use a newly defined subparser as a new namespace of kinds.
In addition you can enable/disable with the subparser usable
``--languages=[+|-]`` option:

.. code-block::console

    $ ctags --options=./linux.ctags --languages=-linux -x --_xformat="%20N %10K %10l"  -o - input.c
	    set_one_prio   function          C
	 SYSCALL_DEFINE3   function          C

.. _optlib_directions:

Direction flags
.........................................................................

.. TESTCASE: Units/flags-langdef-directions.r

As explained in ":ref:`multiple_parsers_directions`" in
":ref:`multiple_parsers`", you can choose direction(s) how a base parser and a
guest parser work together with direction flags.

The following examples are taken from `#1409
<https://github.com/universal-ctags/ctags/issues/1409>`_ submitted by @sgraham on
github Universal Ctags repository.

``input.cc`` and ``input.mojom`` are input files, and have the same
contents::

	ABC();
	int main(void)
	{
	}

C++ parser can capture ``main`` as a function. `Mojom` subparser defined in the
later runs on C++ parser and is for capturing ``ABC``.

shared combination
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
``{shared}`` is specified, for ``input.cc``, both tags capture by C++ parser
and mojom parser are recorded to tags file. For ``input.mojom``, only
tags captured by mojom parser are recorded to tags file.

mojom-shared.ctags:

.. code-block:: ctags
	:emphasize-lines: 1

	--langdef=mojom{base=C++}{shared}
	--map-mojom=+.mojom
	--kinddef-mojom=f,function,functions
	--regex-mojom=/^[ ]+([a-zA-Z]+)\(/\1/f/

.. code-block:: ctags
	:emphasize-lines: 2

	$ ctags --options=mojom-shared.ctags --fields=+l -o - input.cc
	ABC	input.cc	/^ ABC();$/;"	f	language:mojom
	main	input.cc	/^int main(void)$/;"	f	language:C++	typeref:typename:int

.. code-block:: ctags
	:emphasize-lines: 2

	$ ctags --options=mojom-shared.ctags --fields=+l -o - input.mojom
	ABC	input.mojom	/^ ABC();$/;"	f	language:mojom

Mojom parser uses C++ parser internally but tags captured by C++ parser are
dropped in the output.

dedicated combination
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
``{dedicated}`` is specified, for ``input.cc``, only tags capture by C++
parser are recorded to tags file. For ``input.mojom``, both tags capture
by C++ parser and mojom parser are recorded to tags file.

mojom-dedicated.ctags:

.. code-block:: ctags
	:emphasize-lines: 1

	--langdef=mojom{base=C++}{dedicated}
	--map-mojom=+.mojom
	--kinddef-mojom=f,function,functions
	--regex-mojom=/^[ ]+([a-zA-Z]+)\(/\1/f/

.. code-block:: ctags

	$ ctags --options=mojom-dedicated.ctags --fields=+l -o - input.cc
	main	input.cc	/^int main(void)$/;"	f	language:C++	typeref:typename:int

.. code-block:: ctags
	:emphasize-lines: 2-3

	$ ctags --options=mojom-dedicated.ctags --fields=+l -o - input.mojom
	ABC	input.mojom	/^ ABC();$/;"	f	language:mojom
	main	input.mojom	/^int main(void)$/;"	f	language:C++	typeref:typename:int

Mojom parser works only when ``.mojom`` file is given as input.

bidirectional combination
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
``{bidirectional}`` is specified, both tags capture by C++ parser and
mojom parser are recorded to tags file for either input ``input.cc`` and
``input.mojom``.

mojom-bidirectional.ctags:

.. code-block:: ctags
	:emphasize-lines: 1

	--langdef=mojom{base=C++}{bidirectional}
	--map-mojom=+.mojom
	--kinddef-mojom=f,function,functions
	--regex-mojom=/^[ ]+([a-zA-Z]+)\(/\1/f/

.. code-block:: ctags
	:emphasize-lines: 2

	$ ctags --options=mojom-bidirectional.ctags --fields=+l -o - input.cc
	ABC	input.cc	/^ ABC();$/;"	f	language:mojom
	main	input.cc	/^int main(void)$/;"	f	language:C++	typeref:typename:int

.. code-block:: ctags
	:emphasize-lines: 2-3

	$ ctags --options=mojom-bidirectional.ctags --fields=+l -o - input.mojom
	ABC	input.cc	/^ ABC();$/;"	f	language:mojom
	main	input.cc	/^int main(void)$/;"	f	language:C++	typeref:typename:int


.. _optlib2c:

Translating an option file into C source code (optlib2c)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Universal Ctags has an ``optlib2c`` script that translates an option file into C
source code. Your optlib parser can thus easily become a built-in parser.

To add your optlib file, ``foo.ctags``, into ctags do the following steps;

* copy ``foo.ctags`` file on ``optlib/`` directory
* add ``foo.ctags`` on ``OPTLIB2C_INPUT`` variable in ``source.mak``
* add ``fooParser`` on ``PARSER_LIST`` macro variable in ``main/parser_p.h``

You are encouraged to submit your :file:`.ctags` file to our repository on
github through a pull request. See ":ref:`contributions`" for more details.
