ctags Internal API
---------------------------------------------------------------------

Input text stream
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. figure:: input-text-stream.svg
	    :scale: 80%

Macro definitions and function prototypes for handling Input text
stream are in main/read.h. The file exists in exuberant ctags, too.
However, the names of macros and functions are changed when
overhauling ``--line-directive`` option.

Ctags has 3 groups of functions for handling input: input, bypass, and
raw. Parser developers should use input group. The rest of two
are for ctags main part.


`inputFile` type and the functions of input group
......................................................................

`inputFile` is the type for representing the input file and stream for
a parser. Ctags uses a global variable `File` having type `inputFile`
for maintaining the input file and stream.

`fp` and `line` are the essential fields of `File`. `fp` having type
well known `FILE` of stdio. By calling functions of input group
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
`File` global variable without updating `input` and `source` fields of
`File`.


Parsers may not need the functions of this group.  The functions are
used in ctags main part. The functions are used to make pattern
fields of tags file, for example.


The functions of raw group
......................................................................
The functions of this group(`readLineRaw` and `readLineRawWithNoSeek`)
take a parameter having type `FILE *`; and don't touch `File` global
variable.

Parsers may not need the functions of this group.  The functions are
used in ctags main part. The functions are used to load option files,
for example.


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
tag informations to FILE. `makeTagEntry` calls `writeTagEntry` if the
parser does not set `useCork` field. `writeTagEntry` calls one of
three functions, `writeTagsEntry`, `writeXrefEntry` or `writeCtagsEntry`.
One of them is chosen depending on the arguments passed to ctags.

If `useCork` is set, the tag informations goes to a queue on memory.
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
something string.

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

		static int parent = SCOPE_NIL;
		...
		parent = makeTagEntry (&e);

The handle can be used by setting to a `scopeIndex`
field of `current` tag, which is in the scope of `parent`.

.. code-block:: c

		current.extensionFields.scopeIndex = parent;

When passing `current` to `makeTagEntry`, the `scopeIndex` is
refereed for emitting the scope information of `current`.

`scopeIndex` must be set to `SCOPE_NIL` if a tag is not in any scope.
When using `scopeIndex` of `current`, `NULL` must be assigned to both
`current.extensionFields.scope[0]` and
`current.extensionFields.scope[1]`.  `initTagEntry` function does this
initialization internally, so you generally you don't have to write
the initialization explicitly.
