.. _optlib:

Extending ctags with Regex parser(*optlib*)
---------------------------------------------------------------------

:Maintainer: Masatake YAMATO <yamato@redhat.com>

----

.. NOT REVIEWED

Option files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Option file is a file in which command line options are written line
by line. ctags loads it and runs as if the options in the file are
passed in command line.

Following file is an example of option file.
.. code-block::

	# Exclude directories that don't contain real code
	--exclude=Units
		--exclude=tinst-root
		--exclude=Tmain

`#` can be used as a start marker of a line comment.
Whitespaces at the start of lines are ignored during loading.

Preload and optlib are the category of option files.

Preload option file
......................................................................

Preload option files are option files loaded at starting up time.
Which files are loaded at starting up time are very different from
Exuberant-ctags.

At starting up time, Universal-ctags loads files having *.ctags* as
file extension under following statically defined directories(preload
path list):

#. *$HOME/.ctags.d*
#. *$HOMEDRIVE$HOMEPATH/.ctags.d*
#. *.ctags.d*
#. *ctags.d*

ctags visits the directories in the order as listed for loading files.
ctags loads files having *.ctags* as file extension in alphabetical
order(strcmp(3) is used for comparing).

Quoted from man page of Exuberant-ctags::

	FILES
		   /ctags.cnf (on MSDOS, MSWindows only)
		   /etc/ctags.conf
		   /usr/local/etc/ctags.conf
		   $HOME/.ctags
		   $HOME/ctags.cnf (on MSDOS, MSWindows only)
		   .ctags
		   ctags.cnf (on MSDOS, MSWindows only)
				  If any of these configuration files exist, each will
				  be expected to contain a set of default options
				  which are read in the order listed when ctags
				  starts, but before the CTAGS environment variable is
				  read or any command line options are read.  This
				  makes it possible to set up site-wide, personal or
				  project-level defaults. It is possible to compile
				  ctags to read an additional configuration file
				  before any of those shown above, which will be
				  indicated if the output produced by the --version
				  option lists the "custom-conf" feature. Options
				  appearing in the CTAGS environment variable or on
				  the command line will override options specified in
				  these files. Only options will be read from these
				  files.  Note that the option files are read in
				  line-oriented mode in which spaces are significant
				  (since shell quoting is not possible). Each line of
				  the file is read as one command line parameter (as
				  if it were quoted with single quotes). Therefore,
				  use new lines to indicate separate command-line
				  arguments.

Let me explain the differences and their intentions.


Directory oriented configuration management
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Exuberant-ctags provides the way to customize ctags with options like
``--langdef=<LANG>`` and ``--regex-<LANG>``. These options are
powerful and make ctags popular in programmers.

Universal-ctags extends this idea; we have added new options for
defining a parser, and have extended existing options. Defining
a new parser with the options is more than "customizing" in
Universal-ctags.

To make a maintain a parser easier defined with the options, put one
parser to one file. Universal-ctags doesn't load a specified
file. Instead, Universal-ctags loads files having *.ctags* as
extension under specified directories. If you have multiple parser
definitions, put them to different files.

Avoiding troubles about option incompatibility
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Universal-ctags doesn't load any files Exuberant-ctags loads at
starting up. The options of Universal-ctags are different from
Exuberant-ctags. It will cause a trouble that Exuberant-ctags loads
an option file in which a user uses options newly introduced in
Universal-ctags and vice versa.

No system wide configuration
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

To make preload path list short, Universal-ctags loads no option file for
system wide configuration.

Use *.ctags* as file extension
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Extensions *.cnf* and *.conf* are obsolete.
Use *.ctags*, the unified extension only.


Optlib option file
......................................................................

In syntax level, there is no difference between optlib option file
and preload option file; options are written line by line in a file.

Optlib option files are option files not loaded at starting up time
automatically. For loading an optlib option file, specify a pathname
for an optlib option file with ``--options=PATHNAME`` option
explicitly.

Exuberant-ctags has ``--options``. With the option, you can specify
a file to load. Universal-ctags extends the option two aspects.


Specifying a directory
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

If you specify a directory instead of a file as argument for
the option, Universal-ctags load all files having *.ctags*
as extension under the directory in alphabetical order.

Optlib path list
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

For loading a file (or directory) specified in ``--options``, ctags
searches "optlib path list" first if the option argument(pathname)
doesn't start with '/' or '.'. If ctags finds a file, ctags loads
it.

If ctags doesn't find a file in the path list, ctags loads
a file (or directory) at the specified pathname.

By default, optlib path list is empty. To set or add a directory
path to the list, use ``--optlib-dir``.

For setting (adding one after clearing)

	``--optlib-dir=PATH``

For adding

	``--optlib-dir=+PATH``

Tips about writing option file
......................................................................

* ``--quiet --options=NONE`` is for disabling preloading. This phrase
  is used well in Tmain test cases.

.. IN MAN PAGE

* Two options are introduced for debugging the process of loading
  option files.

	``--_echo=MSG``

		Print MSG to standard error immediately.

	``--_force-quit=[NUM]``

		Exit immediately with status specified NUM.

* Universal-ctags has optlib2c command that translator a option file
  into C file. Your optlib parser can be a built-in parser.
  Examples are in *optlib* directory in Universal-ctags source tree.

Long regex flag
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Regex parser is made more useful by adding more kinds of flags
to ``--regex-<LANG>`` expression. As explained in
*ctags.1* man page, ``b``, ``e`` and ``i`` are defined as flags in
exuberant-ctags.

Even if more flags are added like ``x``, ``y``, ``z``,..., users
may not utilize them well because it is difficult to memorize them. In
addition, if many "option libraries" are contributed, we have to
maintain them.

For both users and developers the variety of short flags are just
nightmares.

So universal-ctags now includes an API for defining long flags, which can be
used as aliases for short flags. The long flags requires more typing
but are more readable.

Here is the mapping between the standard short flag names and long flag names:

=========== ===========
short flag  long flag
=========== ===========
b           basic
e           extend
i           icase
=========== ===========

Long flags can be specified with surrounding ``{`` and ``}``.
So the following ``--regex-<LANG>`` expression ::

   --m4-regex=/^m4_define\(\[([^]$\(]+).+$/\1/d,definition/x

is the same as ::

   --m4-regex=/^m4_define\(\[([^]$\(]+).+$/\1/d,definition/{extend}

The characters ``{`` and ``}`` may not be suitable for command line
use, but long flags are mostly intended for option libraries.

The notion for the long flag is also introduced in ``--langdef`` option.

Exclusive flag in regex
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A line read from input files was matched with **all** regular expressions
defined with ``--regex-<LANG>``. Each regular
expression matched successfully emits a tag.

In some cases another policy, exclusive-matching, is preferable to the
all-matching policy. Exclusive-matching means the rest of regular
expressions are not tried if one of regular expressions is matched
successfully,

For specifying exclusive-matching the flags ``exclusive`` (long) and
``x`` (short) were introduced. It is used in *data/optlib/m4.ctags*
for ignoring a line::

	--regex-m4=/#.*(define|undefine|s?include)\>//x
	--regex-m4=/\<dnl.*(define|undefine|s?include)\>//x

Comments are started from ``#`` or ``dnl`` in many use case of m4 language.
With above options ctags can ignore ``define`` in comments.

If an empty name pattern(``//``) is found in ``--regex-<LANG>`` option
ctags warns it as wrong usage of the option. However, the flags
``exclusive`` or ``x`` is specified, the warning is suppressed. This
is imperfect approach for ignoring text insides comments but it may
be better than nothing. Ghost kind is assigned to the empty name
pattern. (See "Ghost kind in regex parser".)

NOTE: This flag doesn't make sense in ``--mline-regex-<LANG>``.


Ghost kind in regex parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If a whitespace is used as a kind letter, it is never printed when
ctags is called with ``--list-kinds`` option.  This kind is
automatically assigned to an empty name pattern.

Normally you don't need to know this.

Passing parameter for long regex flag
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the implemented API long-flags can take a parameters.
Conceptual example::

	--regex-<LANG>=/regexp1/replacement/kind-spec/{transformer=uppercase}
	--regex-<LANG>=/regexp2/replacement/kind-spec/{transformer=lowercase}
	--regex-<LANG>=/regexp2/replacement/kind-spec/{transformer=capitalize}


Scope tracking in a regex parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. IN MAN PAGE

With scope long flag, you can record/track scope context.
A stack is used for tracking the scope context.

`{scope=push}`

	Push the tag captured with a regex pattern to the top of the stack.
	If you don't want to record this tag but just push, use
	`placeholder` long option together.

`{scope=ref}`

	Refer the thing of top of the stack as a scope where
	the tag captured with a regex pattern is.
	The stack is not modified with this specification.
	If the stack is empty, this flag is just ignored.

`{scope=pop}`

	Pop the thing of top of the stack.
	If the stack is empty, this flag is just ignored.

`{scope=clear}`

	Make the stack empty.

`{scope=set}`

	Clear then push.

`{placeholder}`

	Don't print a tag captured with a regex pattern
	to a tag file.
	This is useful when you need to push non-named context
	information to the stack.  Well known non-named scope in C
	language is established with `{`. non-named scope is never
	appeared in tags file as name or scope name.  However, pushing
	it is important to balance `push` and `pop`.

Example 1::

	$ cat /tmp/input.foo
	class foo:
	def bar(baz):
		print(baz)
	class goo:
	def gar(gaz):
		print(gaz)

	$ cat /tmp/foo.ctags
	--langdef=foo
		--map-foo=+.foo
		--regex-foo=/^class[[:blank:]]+([[:alpha:]]+):/\1/c,class/{scope=set}
		--regex-foo=/^[[:blank:]]+def[[:blank:]]+([[:alpha:]]+).*:/\1/d,definition/{scope=ref}

	$ ~/var/ctags/ctags --options=/tmp/foo.ctags -o - /tmp/input.foo
	bar	/tmp/input.foo	/^    def bar(baz):$/;"	d	class:foo
	foo	/tmp/input.foo	/^class foo:$/;"	c
	gar	/tmp/input.foo	/^    def gar(gaz):$/;"	d	class:goo
	goo	/tmp/input.foo	/^class goo:$/;"	c


Example 2::

	$ cat /tmp/input.pp
	class foo {
	include bar
	}

	$ cat /tmp/pp.ctags
	--langdef=pp
		--map-pp=+.pp
		--regex-pp=/^class[[:blank:]]*([[:alnum:]]+)[[[:blank:]]]*\{/\1/c,class,classes/{scope=push}
		--regex-pp=/^[[:blank:]]*include[[:blank:]]*([[:alnum:]]+).*/\1/i,include,includes/{scope=ref}
		--regex-pp=/^[[:blank:]]*\}.*//{scope=pop}{exclusive}

	$ ~/var/ctags/ctags --options=/tmp/pp.ctags -o - /tmp/input.pp
	bar	/tmp/input.pp	/^    include bar$/;"	i	class:foo
	foo	/tmp/input.pp	/^class foo {$/;"	c


NOTE: Giving a scope long flag implies setting `useCork` of the parser
to `TRUE`. See `cork API`.

NOTE: This flag doesn't work well with ``--mline-regex-<LANG>=``.


Override the letter for file kind
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(See also #317.)

Overriding the letter for file kind is not allowed in Universal-ctags.

.. IN MAN PAGE

Don't use `F` as a kind letter in your parser.


Multiline pattern match
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

Newly introduced ``--mline-regex-<LANG>=`` is similar ``--regex-<LANG>``
but the pattern is applied to whole file contents, not line by line.

Next example is based on an issue #219 posted by @andreicristianpetcu::

	$ cat input.java
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

	$ cat spring.ctags
	--langdef=javaspring
	--langmap=javaspring:.java
	--mline-regex-javaspring=/@Subscribe([[:space:]])*([a-z ]+)[[:space:]]*([a-zA-Z]*)\(([a-zA-Z]*)/\3-\4/s,subscription/{mgroup=3}
	--excmd=mixed
	--fields=+ln

	$ ./ctags -o - --options=./spring.ctags input.java
	Event-SomeEvent	input.java	/^public void catchEvent(SomeEvent e)$/;"	s	line:2	language:javaspring
	recover-Exception	input.java	/^    recover(Exception e)$/;"	s	line:10	language:javaspring

``{mgroup=N}``

	This tells the pattern should be applied to whole file
	contents, not line by line.  ``N`` is the number of a group in the
	pattern. The specified group is used to record the line number
	and the pattern of tag. In the above example 3 is
	specified. The start position of the group 3 within the whole
	file contents is used.

``{_advanceTo=N[start|end]}``

	A pattern is applied to whole file contents iteratively.
	This long flag specifies from where the pattern should
	be applied in next iteration when the pattern is matched.
	When a pattern matches, the next pattern application
	starts from the start or end of group ``N``. By default
	it starts from the end of ``N``. If this long flag is not
	given, 0 is assumed for ``N``.


	Let's think about following input
	::

	   def def abc

	Consider two sets of options, foo and bar.

	*foo.ctags*
	::

	   --langdef=foo
	   --langmap=foo:.foo
	   --kinddef-foo=a,something,something
	   --mline-regex-foo=/def *([a-z]+)/\1/a/{mgroup=1}


	*bar.ctags*
	::

	   --langdef=bar
	   --langmap=bar:.bar
	   --kinddef-bar=a,something,something
	   --mline-regex-bar=/def *([a-z]+)/\1/a/{mgroup=1}{_advanceTo=1start}

	*foo.ctags* emits following tags output::

	   def	input.foo	/^def def abc$/;"	a

	*bar.ctgs* emits following tags output::

	   def	input-0.bar	/^def def abc$/;"	a
	   abc	input-0.bar	/^def def abc$/;"	a

	``_advanceTo=1start`` is specified in *bar.ctags*.
	This allows ctags to capture "abc".

	At the first iteration, the patterns of both
	*foo.ctags* and *bar.ctags* match as follows
	::

		0   1       (start)
		v   v
		def def abc
			   ^
			   0,1  (end)

	"def" at the group 1 is captured as a tag in
	both languages. At the next iteration, the positions
	where the pattern matching is applied to are not the
	same in the languages.

	*foo.ctags*
	::

			   0end (default)
			   v
		def def abc


	*bar.ctags*
	::

			1start (as specified in _advanceTo long flag)
			v
		def def abc

	This difference of positions makes the difference of tags output.


NOTE: This flag doesn't work well with scope related flags and ``exclusive`` flags.

.. _extras:


Byte oriented pattern matching with multiple regex tables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

(This is highly experimental feature. This will not go to
the man page of 6.0.)

`--_tabledef-<LANG>` and `--_mtable-regex-<LANG>` options are
experimental, and are for defining a parser using multiple regex
tables. The feature is inspired by `lex`, the fast lexical analyzer
generator, which is a popular tool on Unix environment for writing a
parser, and `RegexLexer` of Pygments. The knowledge about them
help you understand the options.

As usable, let me explain the feature with an example. Consider a
imaginary language "X" has similar syntax with JavaScript; "var" is
used as defining variable(s), , and "/\* ... \*/" makes block comment.

*input.x*
::

   /* BLOCK COMMENT
   var dont_capture_me;
   */
   var a /* ANOTHER BLOCK COMMENT */, b;

Here ctags should capture `a` and `b`.
It is difficult to write a parser ignoring `dont_capture_me` in the comment
with a classical regex parser defined with `--regex-<LANG>=`.

A classical regex parser has no way to know where the input is in
comment or not.

A classical regex parser is line oriented, so capturing `b` will
be hard.

A parser written with `--_tabledef-<LANG>` and `--_mtable-regex-<LANG>`
option(mtable parser) can capture only `a` and `b` well.


Here is the 1st version of X.ctags.
::

   --langdef=X
   --map-X=.x
   --kinddef-X=v,var,variables

Not so interesting.

When writing a mtable parser, you have to think about necessary states
of parsing. About the input the parser should have following
states.

* `toplevel` (initial state)
* `comment` (inside comment)
* `vars` (var statements)

Before enumerating regular expressions, you have to
declare tables for each states with `--_tabledef-<LANG>=<TABLE>` option:

Here is the 2nd version of X.ctags.
::

   --langdef=X
   --map-X=.x
   --kinddef-X=v,var,variables

   --_tabledef-X=toplevel
   --_tabledef-X=comment
   --_tabledef-X=vars

As the part of table, chars in `[0-9a-zA-Z_]` are acceptable.
A mtable parser chooses the first table for each new input.
In `X.ctags`, `toplevel` is the one.


`--_mtable-regex-<LANG>` is an option for adding a regex pattern
to table.

| `--_mtable-regex-<LANG>=<TABLE>/<PATTERN>/<NAME>/<KIND>/LONGFLAGS`

Parameters for `--_mtable-regex-<LANG>` looks complicated. However,
`<PATTERN>`, `<NAME>`, and `<KIND>` are the same as parameters of
`--regex-<LANG>`. `<TABLE>` is the name of a table defined with
`--_tabledef-<LANG>` option.

A regex added to a parser with `--_mtable-regex-<LANG>` is matched
against the input at the current byte position, not line. Even if you
do not specified `^` at the start of the pattern, ctags adds `^` to
the patter automatically. Different from `--regex-<LANG>` option, `^`
does not mean "begging of line" in `--_mtable-regex-<LANG>`.  `^`
means the current byte position in `--_mtable-regex-<LANG>`.


Skipping block comments
......................................................................

The most interesting part if `LONGFLAGS`.

Here is the 3rd version of X.ctags.
::

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

Four `--_mtable-regex-X` liens are added for skipping the block comment.

Let's see the one by one.

For new input, ctags chooses the first pattern of the first table of
the parser.

|    --_mtable-regex-X=toplevel/\/\*//{tenter=comment}

A pattern for `/*` is added to `toplevel` table. It tells ctags
the start of block comment. Backslash chars are used for avoiding chars
(`/` and `*`) evaluated as meta characters. The last `//` means ctags should
not tag `/*`.  `tenter` is a long flag for switching the table. `{tenter=comment}`
means "switch the table from toplevel to comment".

ctags chooses the first pattern of the new table of the parser.

|    --_mtable-regex-X=comment/\*\///{tleave}

A pattern for `*/` tells ctags that `*/` is the end of block comment.

*input.x*
::

   /* BLOCK COMMENT
   var dont_capture_me;
   */
   var a /* ANOTHER BLOCK COMMENT */, b;

The pattern doesn't match for the position just after `/*`. The char
at the position is a whitespace. So ctags tries next pattern in the
same table.

|    --_mtable-regex-X=comment/.//

This pattern matches any one byte; the current position moves one byte
forward. Now the char at the current position is `B`. The first
pattern of the table `*/` still does not match with the input. So
ctags uses next pattern again. When the current position moves to
the `/*` of the 3rd line of input.

|    --_mtable-regex-X=comment/\*\///{tleave}

The pattern match the input finally. In this pattern, `{tleave}` is
specified. This triggers table switching again. `{tleave}` makes
ctags switch the table back to the last table used before doing
`{tenter}`. In this case, toplevel is the table. ctags manages
a stack where references to tables are put. `{tenter}` pushes
the current table to the stack. `{tleave}` pops the table at
the top of the stack and chooses it.

|    --_mtable-regex-X=toplevel/.//

This version of X.ctags does nothing more; toplevel table
ignores all other than the comment starter.



Capturing variables in a sequence
......................................................................

Here is the 4th version of X.ctags.

::

	--langdef=X
	--map-X=.x
	--kinddef-X=v,var,variables

	--_tabledef-X=toplevel
	--_tabledef-X=comment
	--_tabledef-X=vars

	--_mtable-regex-X=toplevel/\/\*//{tenter=comment}
	# NEW
	--_mtable-regex-X=toplevel/var[ \n\t]//{tenter=vars}
	--_mtable-regex-X=toplevel/.//

	--_mtable-regex-X=comment/\*\///{tleave}
	--_mtable-regex-X=comment/.//

	# NEW
	--_mtable-regex-X=vars/;//{tleave}
	--_mtable-regex-X=vars/\/\*//{tenter=comment}
	--_mtable-regex-X=vars/([a-zA-Z][a-zA-Z0-9]*)/\1/v/
	--_mtable-regex-X=vars/.//

1 pattern to `toplevel` and 4 patterns to `vars` are added.

| --_mtable-regex-X=toplevel/var[ \n\t]//{tenter=vars}

The first pattern to `toplevel` intents switching to `vars` table
when `var` keyword is found in the input stream.

|	--_mtable-regex-X=vars/;//{tleave}

`vars` table is for capturing variables. vars table is used
till `;` is found.

|	--_mtable-regex-X=vars/\/\*//{tenter=comment}

Block comments can be in variable definitions:

::

   var a /* ANOTHER BLOCK COMMENT */, b;

To skip block comment in such position, pattern `/*` is matched even
in `vars` table.

|	--_mtable-regex-X=vars/([a-zA-Z][a-zA-Z0-9]*)/\1/v/

This is nothing special: capturing a variable name as
`variable` kind tag.

|	--_mtable-regex-X=vars/.//

This makes ctags ignore the rest like `,`.


Running
......................................................................

.. code-block:: console

	$ cat input.x
	cat input.x
	/* BLOCK COMMENT
	var dont_capture_me;
	*/
	var a /* ANOTHER BLOCK COMMENT */, b;

	$ u-ctags -o - --fields=+n --options=X.ctags input.x
	u-ctags -o - --fields=+n --options=X.ctags input.x
	a	input.x	/^var a \/* ANOTHER BLOCK COMMENT *\/, b;$/;"	v	line:4
	b	input.x	/^var a \/* ANOTHER BLOCK COMMENT *\/, b;$/;"	v	line:4

Fine!

See `puppetManifest` parser as s serious example.
It is the primary parser for testing mtable meta parser.


Conditional tagging with extras
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

If a pattern matching should be done only when an extra is enabled,
mark a pattern with ``{_extra=XNAME}``. Here ``XNAME`` is the name of
extra. You must define ``XNAME`` with ``--_extradef-<LANG>=XNAME,DESCRIPTION`` option
before defining a pattern marked ``{_extra=XNAME}``.

.. code-block:: python

	if __name__ == '__main__':
		do_something()

To capture above lines in a python program(*input.py*), an extra can be used.

.. code-block:: ctags

	--_extradef-Python=main,__main__ entry points
	--regex-Python=/^if __name__ == '__main__':/__main__/f/{_extra=main}

The above optlib(*python-main.ctags*) introduces ``main`` extra to Python parser.
The pattern matching is done only when the ``main`` is enabled.

.. code-block:: ctags

	$ ./ctags --options=python-main.ctags -o - --extras-Python='+{main}' input.py
	__main__	input.py	/^if __name__ == '__main__':$/;"	f

Attaching parser own fields
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

Exuberant-ctags allows one of the specified group in a regex pattern can be
used as a part of the name of a tagEntry. Universal-ctags offers using
the other groups in the regex pattern.

An optlib parser can have its own fields. The groups can be used as a
value of the fields of a tagEntry.

Let's think about *Unknown*, an imaginary language.
Here is a source file(``input.unknown``) written in *Unknown*:

	public func foo(n, m);
	protected func bar(n);
	private func baz(n,...);

With `--regex-Unknown=...` Exuberant-ctags can capture `foo`, `bar`, and `baz`
as names. Universal-ctags can attach extra context information to the
names as values for fields. Let's focus on `bar`. `protected` is a
keyword to control how widely the identifier `bar` can be accessed.
`(n)` is the parameter list of `bar`. `protected` and `(n)` are
extra context information of `bar`.

With following optlib file(``unknown.ctags``)), ctags can attach
`protected` to protection field and `(n)` to signature field.

.. code-block:: ctags

	--langdef=unknown
	--kinddef-unknown=f,func,functions
	--map-unknown=+.unknown

	--_fielddef-unknown=protection,access scope
	--_fielddef-unknown=signature,signatures

	--regex-unknown=/^((public|protected|private) +)?func ([^\(]+)\((.*)\)/\3/f/{_field=protection:\1}{_field=signature:(\4)}

	--fields-unknown=+'{protection}{signature}'

For the line `    protected func bar(n);` you will get following tags output::

	bar	input.unknown	/^protected func bar(n);$/;"	f	protection:protected	signature:(n)

Let's see the detail of ``unknown.ctags``.

.. code-block:: ctags

	--_fielddef-unknown=protection,access scope

`--_fielddef-<LANG>=name,description` defines a new field for a parser
specified by `<LANG>`.  Before defining a new field for the parser,
the parser must be defined with `--langdef=<LANG>`. `protection` is
the field name used in tags output. `access scope` is the description
used in the output of ``--list-fields`` and ``--list-fields=Unknown``.

.. code-block:: ctags

	--_fielddef-unknown=signature,signatures

This defines a field named `signature`.

.. code-block:: ctags

	--regex-unknown=/^((public|protected|private) +)?func ([^\(]+)\((.*)\)/\3/f/{_field=protection:\1}{_field=signature:(\4)}

This option requests making a tag for the name that is specified with the group 3 of the
pattern, attaching the group 1 as a value for `protection` field to the tag, and attaching
the group 4 as a value for `signature` field to the tag. You can use the long regex flag
`_field` for attaching fields to a tag with following notation rule::

  {_field=FIELDNAME:GROUP}


`--fields-<LANG>=[+|-]{FIELDNAME}` can be used to enable or disable specified field.

When defining a new parser own field, it is disabled by default. Enable the
field explicitly to use the field. See :ref:`Parser own fields <parser-own-fields>`
about `--fields-<LANG>` option.

`passwd` parser is a simple example that uses `--fields-<LANG>` option.


.. _capturing_reftag:

Capturing reference tags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NOT REVIEWED YET

To capture a reference tag with an optlib parser, specify a role with
`_role` long regex flag. Let's see an example:

.. code-block:: ctags

	--langdef=FOO
	--kinddef-FOO=m,module,modules
	--_roledef-FOO=m.imported,imported module
	--regex-FOO=/import[ \t]+([a-z]+)/\1/m/{_role=imported}
	--extras=+r
	--fields=+r

See the line, `--regex-FOO=...`.  In this parser `FOO`, a name of
imported module is captured as a reference tag with role `imported`.
A role must be defined before specifying it as value for `_role` flag.
`--_roledef-<LANG>` option is for defining a role.

The parameter of the option comes from three components: a kind
letter, the name of role, and the description of role. The kind letter
comes first.  Following a period, give the role name. The period
represents that the role is defined under the kind specified with the
kind letter.  In the example, `imported` role is defined under
`module` kind specified with `m`.

Of course, the kind specified with the kind letter must be defined
before using `--_roledef-<FOO>` option. `--kinddef-<LANG>` option
is for defining a kind.

The roles are listed with `--list-roles=<LANG>`. The name and
description passed to `--_roledef-<LANG>` option are used in
the output like::

	$ ./ctags --langdef=FOO --kinddef-FOO=m,module,modules \
				--_roledef-FOO='m.imported,imported module' --list-roles=FOO
	#KIND(L/N) NAME     ENABLED DESCRIPTION
	m/module   imported on      imported module


With specifying `_role` regex flag multiple times with different
roles, you can assign multiple roles to a reference tag.
See following input of C language

.. code-block:: C

   i += 1;

An ultra fine grained C parser may capture a variable `i` with
`lvalue` and `incremented`. You can do it with:

.. code-block:: ctags

	--_roledef-C=v.lvalue,locator values
	--_roledef-C=v.incremented,incremeted with ++ operator
	--regex-C=/([a-zA-Z_][a-zA-Z_0-9])+ *+=/\1/v/{_role=lvalue}{_role=incremeted}


Submitting an optlib to universal-ctags project
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You are welcome.

universal-ctags provides a facility for "Option library".
Read "Option library" about the concept and usage first.

Here I will explain how to merge your .ctags into universal-ctags as
part of option library. Here I assume you consider contributing
an option library in which a regex based language parser is defined.
See `How to Add Support for a New Language to Exuberant Ctags (EXTENDING)`_
about the way to how to write a regex based language parser. In this
section I explains the next step.

.. _`How to Add Support for a New Language to Exuberant Ctags (EXTENDING)`: http://ctags.sourceforge.net/EXTENDING.html

I use Swine as the name of programming language which your parser
deals with. Assume source files written in Swine language have a suffix
*.swn*. The file name of option library is *swine.ctags*.


Copyright notice, contact mail address and license term
......................................................................

Put these information at the header of *swine.ctags*.

An example taken from *data/optlib/ctags.ctags* ::

	#
	#
	#  Copyright (c) 2014, Red Hat, Inc.
	#  Copyright (c) 2014, Masatake YAMATO
	#
	#  Author: Masatake YAMATO <yamato@redhat.com>
	#
	# This program is free software; you can redistribute it and/or
	# modify it under the terms of the GNU General Public License
	# as published by the Free Software Foundation; either version 2
	# of the License, or (at your option) any later version.
	#
	# This program is distributed in the hope that it will be useful,
	# but WITHOUT ANY WARRANTY; without even the implied warranty of
	# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	# GNU General Public License for more details.
	#
	# You should have received a copy of the GNU General Public License
	# along with this program; if not, write to the Free Software
	# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
	# USA.
	#
	#
	...

"GPL version 2 or later version" is needed here.  Option file is not
linked to ctags command. However, I have a plan to write a translator
which generates *.c* file from a given option file. As the result the
*.c* file is built into *ctags* command. In such a case "GPL version 2
or later version" may be required.

*Units* test cases
......................................................................

We, universal-ctags developers don't have enough time to learn all
languages supported by ctags. In other word, we cannot review the
code. Only test cases help us to know whether a contributed option
library works well or not. We may reject any contribution without
a test case.

Read "Using *Units*" about how to write *Units* test
cases.  Don't write one big test case. Some smaller cases are helpful
to know about the intent of the contributor.

* *Units/sh-alias.d*
* *Units/sh-comments.d*
* *Units/sh-quotes.d*
* *Units/sh-statements.d*

are good example of small test cases.
Big test cases are good if smaller test cases exist.

See also *parser-m4.r/m4-simple.d* especially *parser-m4.r/m4-simple.d/args.ctags*.
Your test cases need ctags having already loaded your option
library, swine.ctags. You must specify loading it in the
test case own *args.ctags*.

Assume your test name is *swine-simile.d*. Put ``--option=swine`` in
*Units/swine-simile.d/args.ctags*.

Makefile.in
......................................................................
Add your optlib file, *swine.ctags* to ``PRELOAD_OPTLIB`` variable of
*Makefile.in*.


If you don't want your optlib loaded automatically when ctags starting up,
put your optlib file to ``OPTLIB`` of *Makefile.in* instead of
``PRELOAD_OPTLIB``.

Verification
......................................................................

Let's verify all your work here.

1. Run the tests and check whether your test case is passed or failed::

	$ make units

2. Verify your files are installed as expected::

	$ mkdir /tmp/tmp
	$ ./configure --prefix=/tmp/tmp
	$ make
	$ make install
	$ /tmp/tmp/ctags -o - --option=swine something_input.swn


Pull-request
......................................................................

Please, consider submitting your well written optlib parser to
Universal-ctags. Your *.ctags* is treasure and can be shared as a
first class software component in universal-ctags.

Pull-requests are welcome.
