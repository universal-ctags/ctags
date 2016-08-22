ctags Internal API
---------------------------------------------------------------------

Input text stream
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. figure:: input-text-stream.svg
	    :scale: 80%

Function prototypes for handling input text stream are declared in
main/read.h. The file exists in exuberant ctags, too.  However, the
names functions are changed when overhauling ``--line-directive``
option. (In addition macros were converted to functions for making
data structures for the input text stream opaque.)

Ctags has 3 groups of functions for handling input: input, bypass, and
raw. Parser developers should use input group. The rest of two
are for ctags main part.


.. _inputFile:

`inputFile` type and the functions of input group
......................................................................

(The original version of this sub sub sub section was written
 before `inputFile` type and `File` variable are made private. )

`inputFile` is the type for representing the input file and stream for
a parser. It was declared in main/read.h but now it is defined in
main/read.c.

Ctags uses a file static variable `File` having type `inputFile` for
maintaining the input file and stream. `File` is also defined in
main/read.c as `inputFile` is.

`fp` and `line` are the essential fields of `File`. `fp` having type
well known `MIO` declared in main/mio.h. By calling functions of input group
(`getcFromInputFile` and `readLineFromInputFile`), a parser gets input
text from `fp`.

The functions of input group updates fields `input` and `source` of `File`
These two fields has type `inputFileInfo`. These two fields are for mainly
tracking the name of file and the current line number. Usually ctags uses
only `input` field. `source` is used only when `#line` directive is found
in the current input text stream.

A case when a tool generates the input file from another file, a tool
can record the original source file to the generated file with using
the `#line` directive. `source` is used for tracking/recording the
informations appeared on #line directives.

Regex pattern matching are also done behind calling the functions of
this group.


The functions of bypass group
......................................................................
The functions of bypass group (`readLineFromBypass` and
`readLineFromBypassSlow`) are used for reading text from `fp` field of
`File` static variable without updating `input` and `source` fields of
`File`.


Parsers may not need the functions of this group.  The functions are
used in ctags main part. The functions are used to make pattern
fields of tags file, for example.


The functions of raw group
......................................................................
The functions of this group(`readLineRaw` and `readLineRawWithNoSeek`)
take a parameter having type `MIO`; and don't touch `File` static
variable.

Parsers may not need the functions of this group.  The functions are
used in ctags main part. The functions are used to load option files,
for example.


promise API
......................................................................
(Currently the tagging via promise API is disabled by default.
 Use `--extra=+s` optoin for enabling it.)

Background and Idea
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
More than one programming languages can be used in one input text stream.
promise API is introduced for running sub (guest) parser in the specified area
of input text stream from the root (host) parser.

e.g. Code written in c language (C code) is embedded
in code written in Yacc language (Yacc code). Let's think about this
input stream.

.. code-block:: yacc

   /* foo.y */
    %token
	    END_OF_FILE	0
	    ERROR		255
	    BELL		1

    %{
    /* C language */
    int counter;
    %}
    %right	EQUALS
    %left	PLUS MINUS
    ...
    %%
    CfgFile		:	CfgEntryList
			    { InterpretConfigs($1); }
		    ;

    ...
    %%
    int
    yyerror(char *s)
    {
	(void)fprintf(stderr,"%s: line %d of %s\n",s,lineNum,
					    (scanFile?scanFile:"(unknown)"));
	if (scanStr)
	    (void)fprintf(stderr,"last scanned symbol is: %s\n",scanStr);
	return 1;
    }

In the input the area started from `%{` to `%}` and the area started from
the second `%%` to the end of file are written in C. Yacc can be called
host language, and C can be called guest language.

Ctags may choose the Yacc parser for the input. However, the parser
doesn't know about C syntax. Implementing C parser in the Yacc parser
is one of approach. However, ctags has already C parser.  The Yacc
parser should utilize the existing C parser. The promise API allows this.

Usage
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

See a commit titled with "Yacc: run C parser in the areas where code
is written in C".  I applied promise API to the Yacc parser.

The parser for host language must track and record the `start` and the
`end` of a guest language. Pairs of `line number` and `byte offset`
represents the `start` and `end`. When the `start` and `end` are
fixed, call `makePromise` with (1) the guest parser name, (2) start,
and (3) end. (This description is a bit simplified the real usage.)


Let's see the actual code from parsers/yacc.c.

.. code-block:: c

	struct cStart {
		unsigned long input;
		unsigned long source;
	};

The both two fields are for recording `start`. `input` field
is for recording the value returned from `getInputLineNumber`.
`source` is for `getSourceLineNumber`. See `inputFile`_ for the
difference of the two.

`enter_c_prologue` shown in the next is a function called when `%{` is
found in the current input text stream. Remember, in yacc syntax, `%{`
is a marker of C code area.

.. code-block:: c

    static void enter_c_prologue (const char *line CTAGS_ATTR_UNUSED,
				 const regexMatch *matches CTAGS_ATTR_UNUSED,
				 unsigned int count CTAGS_ATTR_UNUSED,
				 void *data)
    {
	   struct cStart *cstart = data;


	   readLineFromInputFile ();
	   cstart->input  = getInputLineNumber ();
	   cstart->source = getSourceLineNumber ();
    }


The function just records the start line.  It calls
`readLineFromInputFile` because the C code may start the next line of
the line where the marker is.

`leave_c_prologue` shown in the next is a function called when `%}`,
the end marker of C code area is found in the current input text stream.

.. code-block:: c

    static void leave_c_prologue (const char *line CTAGS_ATTR_UNUSED,
				 const regexMatch *matches CTAGS_ATTR_UNUSED,
				 unsigned int count CTAGS_ATTR_UNUSED,
				 void *data)
    {
	   struct cStart *cstart = data;
	   unsigned long c_end;

	   c_end = getInputLineNumber ();
	   makePromise ("C", cstart->input, 0, c_end, 0, cstart->source);
    }

After recording the line number of the end of the C code area,
`leave_c_prologue` calls `makePromise`.

Of course "C" stands for C language, the name of guest parser.
Available parser names can be listed by running ctags with
`--list-languages` option. Two `0` are specified as the 3rd and 5th
arguments. They are byte offset of the C language area. The first one
is for start and the second one is for the end.

Internal design
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

A host parser cannot run a guest parser directly. What the host parser
can do is just asking the ctags main part scheduling of running the
guest parser for specified area which defined with the `start` and
`end`. These scheduling requests are called promises.

After running the host parser, before closing the input stream, the
ctags main part checks the existence of promise(s). If there is, the
main part makes a sub input stream and run the guest parser specified
in the promise. The sub input stream is made from the original input
stream by narrowing as requested in the promise. The main part
iterates the above process till there is no promise.

Theoretically a guest parser can make more promises. It is just
scheduled.  However, I have never tested such case.

Why not running the guest parser directly from the context of the host
parser? Remember many parsers have their own file static variables. If
a parser is called from the parser, the variables may be crashed.


Automatic parser guessing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Managing regular expression parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Parser combination
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Parser written in C
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tag entfry info
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Output tag stream
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. figure:: output-tag-stream.svg
	    :scale: 80%

Ctags provides `makeTagEntry` to parsers as an entry point for writing
tag informations to MIO. `makeTagEntry` calls `writeTagEntry` if the
parser does not set `useCork` field. `writeTagEntry` calls one of
three functions, `writeTagsEntry`, `writeXrefEntry` or `writeCtagsEntry`.
One of them is chosen depending on the arguments passed to ctags.

If `useCork` is set, the tag information goes to a queue on memory.
The queue is flushed when `useCork` in unset. See `cork API` for more
details.

cork API
......................................................................

Background and Idea
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
cork API is introduced for recording scope information easier.

Before introducing cork, a scope information must be recorded as
strings. It is flexible but memory management is required.
Following code is taken from clojure.c(with modifications).

.. code-block:: c

		if (vStringLength (parent) > 0)
		{
			current.extensionFields.scope[0] = ClojureKinds[K_NAMESPACE].name;
			current.extensionFields.scope[1] = vStringValue (parent);
		}

		makeTagEntry (&current);

`parent`, values stored to `scope [0]` and `scope [1]` are all
kind of strings.

cork API provides more solid way to hold scope information. cork API
expects `parent`, which represents scope of a tag(`current`)
currently parser dealing, is recorded to a *tags* file before recording
the `current` tag via `makeTagEntry` function.

For passing the information about `parent` to `makeTagEntry`,
`tagEntryInfo` object was created. It was used just for recording; and
freed after recording.  In cork API, it is not freed after recording;
a parser can reused it as scope information.

How to use
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

See a commit titled with "clojure: use cork". I applied cork
API to the clojure parser.

cork can be enabled and disabled per parser.
cork is disabled by default. So there is no impact till you
enables it in your parser.

`useCork` field is introduced in `parserDefinition` type:

.. code-block:: c

		typedef struct {
		...
				boolean useCork;
		...
		} parserDefinition;

Set `TRUE` to `useCork` like:

.. code-block:: c

    extern parserDefinition *ClojureParser (void)
    {
	    ...
	    parserDefinition *def = parserNew ("Clojure");
	    ...
	    def->useCork = TRUE;
	    return def;
    }

When ctags running a parser with `useCork` being `TRUE`, all output
requested via `makeTagEntry` function calling is stored to an internal
queue, not to `tags` file.  When parsing an input file is done, the
tag information stored automatically to the queue are flushed to
`tags` file in batch.

When calling `makeTagEntry` with a `tagEntryInfo` object(`parent`),
it returns an integer. The integer can be used as handle for referring
the object after calling.


.. code-block:: c

		static int parent = CORK_NIL;
		...
		parent = makeTagEntry (&e);

The handle can be used by setting to a `scopeIndex`
field of `current` tag, which is in the scope of `parent`.

.. code-block:: c

		current.extensionFields.scopeIndex = parent;

When passing `current` to `makeTagEntry`, the `scopeIndex` is
refereed for emitting the scope information of `current`.

`scopeIndex` must be set to `CORK_NIL` if a tag is not in any scope.
When using `scopeIndex` of `current`, `NULL` must be assigned to both
`current.extensionFields.scope[0]` and
`current.extensionFields.scope[1]`.  `initTagEntry` function does this
initialization internally, so you generally you don't have to write
the initialization explicitly.

Automatic full qualified tag generation
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

If a parser uses the cork for recording and emitting scope
information, ctags can reuse it for generating full qualified(FQ)
tags. Set `requestAutomaticFQTag` field of `parserDefinition` to
`TRUE` then the main part of ctags emits FQ tags on behalf of the parser
if `--extra=+q` is given.

An example can be found in DTS parser:

.. code-block:: c

    extern parserDefinition* DTSParser (void)
    {
	    static const char *const extensions [] = { "dts", "dtsi", NULL };
	    parserDefinition* const def = parserNew ("DTS");
	    ...
	    def->requestAutomaticFQTag = TRUE;
	    return def;
    }

Setting `requestAutomaticFQTag` to `TRUE` implies setting
`useCork` to `TRUE`.
