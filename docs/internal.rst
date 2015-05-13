ctags Internal API
============================================================

Input text stream
------------------------------------------------------------

Automatic parser guessing
------------------------------------------------------------

Managing regular expression parsers
------------------------------------------------------------

Parser combination
------------------------------------------------------------

Parser written in C
------------------------------------------------------------

Tag entfry info
------------------------------------------------------------

Output tag stream
------------------------------------------------------------

.. figure:: output-tag-stream.svg
	    :scale: 80%

Ctags provides `makeTagEntry` to parsers as an entry point for writing
tag informations to FILE. `makeTagEntry` calls `writeTagEntry` if the
parser does not set `use_cork` field. `writeTagEntry` calls one of
three functions, `writeTagsEntry`, `writeXrefEntry` or `writeCtagsEntry`.
One of them is chosen depending on the arguments passed to ctags.

If `use_cork` is set, the tag informations goes to a queue on memroy.
The queu is flushed when `use_cork` in unset. See `cork API` for more
details.

cork API
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Background and Idea
......................................................................
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
......................................................................

See a commit titled with "clojure: use cork". I applied cork
API to the clojure parser.

cork can be enabled and disabled per parser.
cork is disabled by default. So there is no impact till you
enables it in your parser.

`use_cork` field is introduced in `parserDefinition` type:

.. code-block:: c

		typedef struct {
		...
				boolean use_cork;
		...
		} parserDefinition;

Set `TRUE` to `use_cork` like:

.. code-block:: c

    extern parserDefinition *ClojureParser (void)
    {
	    ...
	    parserDefinition *def = parserNew ("Clojure");
	    ...
	    def->use_cork = TRUE;
	    return def;
    }

When ctags running a parser with use_cork being `TRUE`, all output
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

The handle can be used by setting to a `scope_index`
field of `current` tag, which is in the scope of `parent`.

.. code-block:: c

		current.extensionFields.scope_index = parent;

When passing `current` to `makeTagEntry`, the `scope_index` is
refereed for emitting the scope information of `current`.

`scope_index` must be set to `SCOPE_NIL` if a tag is not in any scope.
When using `scope_index` of `current`, `NULL` must be assigned to both
`current.extensionFields.scope[0]` and
`current.extensionFields.scope[1]`.  `initTagEntry` function does this
initialization internally, so you generally you don't have to write
the initialization explicitly.
