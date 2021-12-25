.. _ctags-optlib(7):

==============================================================
ctags-optlib
==============================================================

Universal Ctags parser definition language

:Version: 5.9.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** [options] [file(s)]
|	**etags** [options] [file(s)]

DESCRIPTION
-----------

*Exuberant Ctags*, the ancestor of *Universal Ctags*, has provided
the way to define a new parser from command line.  Universal Ctags
extends and refines this feature. *optlib parser* is the name for such
parser in Universal Ctags. "opt" intends a parser is defined with
combination of command line options. "lib" intends an optlib parser
can be more than ad-hoc personal configuration.

This man page is for people who want to define an optlib parser. The
readers should read :ref:`ctags(1) <ctags(1)>` of Universal Ctags first.

Following options are for defining (or customizing) a parser:

* ``--langdef=<name>``
* ``--map-<LANG>=[+|-]<extension>|<pattern>``
* ``--kinddef-<LANG>=<letter>,<name>,<description>``
* ``--regex-<LANG>=/<line_pattern>/<name_pattern>/<kind-spec>/[<flags>]``
* ``--mline-regex-<LANG>=/<line_pattern>/<name_pattern>/<kind-spec>/[<flags>]``

Following options are for controlling loading parser definition:

* ``--options=<pathname>``
* ``--options-maybe=<pathname>``
* ``--optlib-dir=[+]<directory>``

The design of options and notations for defining a parser in
Exuberant Ctags may focus on reducing the number of typing by user.
Reducing the number of typing is important for users who want to
define (or customize) a parser quickly.

On the other hand, the design in Universal Ctags focuses on
maintainability. The notation of Universal Ctags is redundant than
that of Exuberant Ctags; the newly introduced kind should be declared
explicitly, (long) names are approved than one-letter flags
specifying kinds, and naming rules are stricter.

This man page explains only stable options and flags.  Universal Ctags
also introduces experimental options and flags which have names starting
with ``_``. For documentation on these options and flags, visit
Universal Ctags web site at https://ctags.io/.


Storing a parser definition to a file
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Though it is possible to define a parser from command line, you don't
want to type the same command line each time when you need the parser.
You can store options for defining a parser into a file.

ctags loads files (preload files) listed in "FILES"
section of :ref:`ctags(1) <ctags(1)>` at program starting up. You can put your parser
definition needed usually to the files.

``--options=<pathname>``, ``--options-maybe=<pathname>``, and
``--optlib-dir=[+]<directory>`` are for loading optlib files you need
occasionally. See "Option File Options" section of :ref:`ctags(1) <ctags(1)>` for
these options.

As explained in "FILES" section of :ref:`ctags(1) <ctags(1)>`, options for defining a
parser listed line by line in an optlib file. Prefixed white spaces are
ignored. A line starting with '#' is treated as a comment.  Escaping
shell meta character is not needed.

Use ``.ctags`` as file extension for optlib file. You can define
multiple parsers in an optlib file but it is better to make a file for
each parser definition.

``--_echo=<msg>`` and ``--_force-quit=<num>`` options are for debugging
optlib parser.


Overview for defining a parser
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1. Design the parser

   You need know both the target language and the ctags'
   concepts (definition, reference, kind, role, field, extra). About
   the concepts, :ref:`ctags(1) <ctags(1)>` of Universal Ctags may help you.

2. Give a name to the parser

   Use ``--langdef=<name>`` option. *<name>* is referred as *<LANG>* in
   the later steps.

3. Give a file pattern or file extension for activating the parser

   Use ``--map-<LANG>=[+|-]<extension>|<pattern>``.

4. Define kinds

   Use ``--kinddef-<LANG>=<letter>,<name>,<description>`` option.
   Universal Ctags introduces this option.  Exuberant Ctags doesn't
   have. In Exuberant Ctags, a kind is defined as a side effect of
   specifying ``--regex-<LANG>=`` option. So user doesn't have a
   chance to recognize how important the definition of kind.

5. Define patterns

   Use ``--regex-<LANG>=/<line_pattern>/<name_pattern>/<kind-spec>/[<flags>]``
   option for a single-line regular expression. You can also use
   ``--mline-regex-<LANG>=/<line_pattern>/<name_pattern>/<kind-spec>/[<flags>]``
   option for a multi-line regular expression.

   As *<kind-spec>*, you can use the one-letter flag defined with
   ``--kinddef-<LANG>=<letter>,<name>,<description>`` option.

OPTIONS
------------

``--langdef=<name>``
	Defines a new user-defined language, *<name>*, to be parsed with regular
	expressions. Once defined, *<name>* may be used in other options taking
	language names.

	*<name>* must consist of alphanumeric characters, '``#``', or '``+``'
	('[a-zA-Z0-9#+]+'). The graph characters other than '``#``' and
	'``+``' are disallowed (or reserved). Some of them (``[-=:{.]``) are
	disallowed because they can make the command line parser of
	ctags confused. The rest of them are just
	reserved for future extending ctags.

	``all`` is an exception.  ``all`` as *<name>* is not acceptable. It is
	a reserved word. See the description of
	``--kinds-(<LANG>|all)=[+|-](<kinds>|*)`` option in :ref:`ctags(1) <ctags(1)>` about how the
	reserved word is used.

	The names of built-in parsers are capitalized. When
	ctags evaluates an option in a command line, and
	chooses a parser, ctags uses the names of
	parsers in a case-insensitive way. Therefore, giving a name
	started from a lowercase character doesn't help you to avoid the
	parser name confliction. However, in a tags file,
	ctags prints parser names in a case-sensitive
	way; it prints a parser name as specified in ``--langdef=<name>``
	option.  Therefore, we recommend you to give a name started from a
	lowercase character to your private optlib parser. With this
	convention, people can know where a tag entry in a tag file comes
	from a built-in parser or a private optlib parser.

``--kinddef-<LANG>=<letter>,<name>,<description>``
	Define a kind for *<LANG>*.
	Be not confused this with ``--kinds-<LANG>``.

	*<letter>* must be an alphabetical character ('[a-zA-EG-Z]')
	other than "F". "F" has been reserved for representing a file
	since Exuberant Ctags.

	*<name>* must start with an alphabetic character, and the rest
	must  be alphanumeric ('[a-zA-Z][a-zA-Z0-9]*'). Do not use
	"file" as *<name>*. It has been reserved for representing a file
	since Exuberant Ctags.

	Note that using a number character in a *<name>* violates the
	version 2 of tags file format though ctags
	accepts it. For more detail, see :ref:`tags(5) <tags(5)>`.

	*<description>* comes from any printable ASCII characters. The
	exception is ``{`` and ``\``. ``{`` is reserved for adding flags
	this option in the future. So put ``\`` before ``{`` to include
	``{`` to a description. To include ``\`` itself to a description,
	put ``\`` before ``\``.

	Both *<letter>*, *<name>* and their combination must be unique in
	a *<LANG>*.

	This option is newly introduced in Universal Ctags.  This option
	reduces the typing defining a regex pattern with
	``--regex-<LANG>=``, and keeps the consistency of kind
	definitions in a language.

	The *<letter>* can be used as an argument for ``--kinds-<LANG>``
	option to enable or disable the kind. Unless ``K`` field is
	enabled, the *<letter>* is used as value in the "kind" extension
	field in tags output.

	The *<name>* surrounded by braces can be used as an argument for
	``--kind-<LANG>`` option. If ``K`` field is enabled, the *<name>*
	is used as value in the "kind" extension field in tags output.

	The *<description>* and *<letter>* are listed in ``--list-kinds``
	output. All three elements of the kind-spec are listed in
	``--list-kinds-full`` output. Don't use braces in the
	*<description>*. They will be used meta characters in the future.

``--regex-<LANG>=/<line_pattern>/<name_pattern>/<kind-spec>/[<flags>]``
	Define a single-line regular expression.

	The */<line_pattern>/<name_pattern>/* pair defines a regular expression
	replacement pattern, similar in style to ``sed`` substitution
	commands, ``s/regexp/replacement/``, with which to generate tags from source files mapped to
	the named language, *<LANG>*, (case-insensitive; either a built-in
	or user-defined language).

	The regular expression, *<line_pattern>*, defines
	an extended regular expression (roughly that used by egrep(1)),
	which is used to locate a single source line containing a tag and
	may specify tab characters using ``\t``.

	When a matching line is
	found, a tag will be generated for the name defined by
	*<name_pattern>*, which generally will contain the special
	back-references ``\1`` through ``\9`` to refer to matching sub-expression
	groups within *<line_pattern>*.

	The '``/``' separator characters shown in the
	parameter to the option can actually be replaced by any
	character. Note that whichever separator character is used will
	have to be escaped with a backslash ('``\``') character wherever it is
	used in the parameter as something other than a separator. The
	regular expression defined by this option is added to the current
	list of regular expressions for the specified language unless the
	parameter is omitted, in which case the current list is cleared.

	Unless modified by *<flags>*, *<line_pattern>* is interpreted as a POSIX
	extended regular expression. The *<name_pattern>* should expand for all
	matching lines to a non-empty string of characters, or a warning
	message will be reported unless ``{placeholder}`` regex flag is
	specified.

	A kind specifier (*<kind-spec>*) for tags matching regexp may
	follow *<name_pattern>*, which will determine what kind of tag is
	reported in the ``kind`` extension field (see :ref:`tags(5) <tags(5)>`).

	*<kind-spec>* has two forms: *one-letter form* and *full form*.

	The	one-letter form in the form of ``<letter>``. It just refers a kind
	*<letter>* defined with ``--kinddef-<LANG>``. This form is recommended in
	Universal Ctags.

	The full form of *<kind-spec>* is in the form of
	``<letter>,<name>,<description>``. 	Either the kind *<name>* and/or the
	*<description>* can be omitted. See the description of
	``--kinddef-<LANG>=<letter>,<name>,<description>`` option about the
	elements.

	The full form is supported only for keeping the compatibility with Exuberant
	Ctags which does not have ``--kinddef-<LANG>`` option. Supporting the
	form will be removed from Universal Ctags in the future.

	.. MEMO: the following line is commented out
		If *<kind-spec>* is omitted, it defaults to ``r,regex``.

	About *<flags>*, see "FLAGS FOR ``--regex-<LANG>`` OPTION".

	For more information on the regular expressions used by
	ctags, see either the regex(5,7) man page, or
	the GNU info documentation for regex (e.g. "``info regex``").

``--list-regex-flags``
	Lists the flags that can be used in ``--regex-<LANG>`` option.

``--list-mline-regex-flags``
	Lists the flags that can be used in ``--mline-regex-<LANG>`` option.

``--mline-regex-<LANG>=/<line_pattern>/<name_pattern>/<kind-spec>/[<flags>]``
	Define a multi-line regular expression.

	This option is similar to ``--regex-<LANG>`` option except the pattern is
	applied to the whole file’s contents, not line by line.

``--_echo=<message>``
	Print *<message>* to the standard error stream.  This is helpful to
	understand (and debug) optlib loading feature of Universal Ctags.

``--_force-quit[=<num>]``
	Exits immediately when this option is processed.  If *<num>* is used
	as exit status. The default is 0.  This is helpful to debug optlib
	loading feature of Universal Ctags.


FLAGS FOR ``--regex-<LANG>`` OPTION
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can specify more than one flag, ``<letter>|{<name>}``, at the end of ``--regex-<LANG>`` to
control how Universal Ctags uses the pattern.

Exuberant Ctags uses a *<letter>* to represent a flag. In
Universal Ctags, a *<name>* surrounded by braces (name form) can be used
in addition to *<letter>*. The name form makes a user reading an optlib
file easier.

The most of all flags newly added in Universal Ctags
don't have the one-letter representation. All of them have only the name
representation. ``--list-regex-flags`` lists all the flags.

``basic`` (one-letter form ``b``)
	The pattern is interpreted as a POSIX basic regular expression.

``exclusive`` (one-letter form ``x``)
	Skip testing the other patterns if a line is matched to this
	pattern. This is useful to avoid using CPU to parse line comments.

``extend`` (one-letter form ``e``)
	The pattern is interpreted as a POSIX extended regular
	expression (default).

``pcre2`` (one-letter form ``p``, experimental)
	The pattern is interpreted as a PCRE2 regular expression explained
	in pcre2syntax(3).  This flag is available only if the ctags is
	built with ``pcre2`` library. See the output of
	``--list-features`` option to know whether your ctags is
	built-with ``pcre2`` or not.

``icase`` (one-letter form ``i``)
	The regular expression is to be applied in a case-insensitive
	manner.

``placeholder``
	Don't emit a tag captured with a regex pattern.  The replacement
	can be an empty string.  See the following description of
	``scope=...`` flag about how this is useful.

``scope=(ref|push|pop|clear|set|replace)``

	Specify what to do with the internal scope stack.

	A parser programmed with ``--regex-<LANG>`` has a stack (scope
	stack) internally. You can use it for tracking scope
	information. The ``scope=...`` flag is for manipulating and
	utilizing the scope stack.

	If ``{scope=push}`` is specified, a tag captured with
	``--regex-<LANG>`` is pushed to the stack. ``{scope=push}``
	implies ``{scope=ref}``.

	You can fill the scope field (``scope:``) of captured tag with
	``{scope=ref}``. If ``{scope=ref}`` flag is given,
	ctags attaches the tag at the top to the tag
	captured with ``--regex-<LANG>`` as the value for the ``scope:``
	field.

	ctags pops the tag at the top of the stack when
	``--regex-<LANG>`` with ``{scope=pop}`` is matched to the input
	line.

	Specifying ``{scope=clear}`` removes all the tags in the scope.
	Specifying ``{scope=set}`` removes all the tags in the scope, and
	then pushes the captured tag as ``{scope=push}`` does.

	``{scope=replace}`` does the three things sequentially. First it
	does the same as ``{scope=pop}``, then fills the ``scope:`` field
	of the tag captured with ``--regex-<LANG>``, and pushes the tag to
	the scope stack as if ``{scope=push}`` was given finally.
	You cannot specify another scope action together with
	``{scope=replace}``.

	You don't want to specify ``{scope=pop}{scope=push}`` as an
	alternative to ``{scope=replace}``; ``{scope=pop}{scope=push}``
	fills the ``scope:`` field of the tag captured with ``--regex-<LANG>``
	first, then pops the tag at the top of the stack, and pushes
	the captured tag to the scope stack finally. The timing when
	filling the end field is different between ``{scope=replace}`` and
	``{scope=pop}{scope=push}``.

	In some cases, you may want to use ``--regex-<LANG>`` only for its
	side effects: using it only to manipulate the stack but not for
	capturing a tag. In such a case, make *<name_pattern>* component of
	``--regex-<LANG>`` option empty while specifying ``{placeholder}``
	as a regex flag. For example, a non-named tag can be put on
	the stack by giving a regex flag "``{scope=push}{placeholder}``".

	You may wonder what happens if a regex pattern with
	``{scope=ref}`` flag matches an input line but the stack is empty,
	or a non-named tag is at the top. If the regex pattern contains a
	``{scope=ref}`` flag and the stack is empty, the ``{scope=ref}``
	flag is ignored and nothing is attached to the ``scope:`` field.

	If the top of the stack contains an unnamed tag,
	ctags searches deeper into the stack to find the
	top-most named tag. If it reaches the bottom of the stack without
	finding a named tag, the ``{scope=ref}`` flag is ignored and
	nothing is attached to the ``scope:`` field.

	When a named tag on the stack is popped or cleared as the side
	effect of a pattern matching, ctags attaches the
	line number of the match to the ``end:`` field of
	the named tag.

	ctags clears all of the tags on the stack when it
	reaches the end of the input source file. The line number of the
	end is attached to the ``end:`` field of the cleared tags.

``warning=<message>``
	print the given *<message>* at WARNING level

``fatal=<message>``
	print the given *<message>* and exit

EXAMPLES
-------------

Perl Pod
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is the definition (pod.ctags) used in ctags for parsing Pod
(https://perldoc.perl.org/perlpod.html) file.

.. code-block:: ctags

   --langdef=pod
   --map-pod=+.pod

   --kinddef-pod=c,chapter,chapters
   --kinddef-pod=s,section,sections
   --kinddef-pod=S,subsection,subsections
   --kinddef-pod=t,subsubsection,subsubsections

   --regex-pod=/^=head1[ \t]+(.+)/\1/c/
   --regex-pod=/^=head2[ \t]+(.+)/\1/s/
   --regex-pod=/^=head3[ \t]+(.+)/\1/S/
   --regex-pod=/^=head4[ \t]+(.+)/\1/t/

Using scope regex flags
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's think about writing a parser for a very small subset of the Ruby
language.

input source file (``input.srb``)::

	class Example
	  def methodA
		puts "in class_method"
	  end
	  def methodB
		puts "in class_method"
	  end
	end

The parser for the input should capture ``Example`` with ``class`` kind,
``methodA``, and ``methodB`` with ``method`` kind. ``methodA`` and ``methodB``
should have ``Example`` as their scope. ``end:`` fields of each tag
should have proper values.

optlib file (``sub-ruby.ctags``):

.. code-block:: ctags

	--langdef=subRuby
	--map-subRuby=.srb
	--kinddef-subRuby=c,class,classes
	--kinddef-subRuby=m,method,methods
	--regex-subRuby=/^class[ \t]+([a-zA-Z][a-zA-Z0-9]+)/\1/c/{scope=push}
	--regex-subRuby=/^end///{scope=pop}{placeholder}
	--regex-subRuby=/^[ \t]+def[ \t]+([a-zA-Z][a-zA-Z0-9_]+)/\1/m/{scope=push}
	--regex-subRuby=/^[ \t]+end///{scope=pop}{placeholder}

command line and output::

	$ ctags --quiet --fields=+eK \
	--options=./sub-ruby.ctags -o - input.srb
	Example	input.srb	/^class Example$/;"	class	end:8
	methodA	input.srb	/^  def methodA$/;"	method	class:Example	end:4
	methodB	input.srb	/^  def methodB$/;"	method	class:Example	end:7


SEE ALSO
--------

The official Universal Ctags web site at:

https://ctags.io/

:ref:`ctags(1) <ctags(1)>`, :ref:`tags(5) <tags(5)>`, regex(3), regex(7), egrep(1), pcre2syntax(3)

AUTHOR
------

Universal Ctags project
https://ctags.io/
(This man page partially derived from :ref:`ctags(1) <ctags(1)>` of
Executable-ctags)

Darren Hiebert <dhiebert@users.sourceforge.net>
http://DarrenHiebert.com/
