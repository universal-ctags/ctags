.. _output-json:

======================================================================
JSON output
======================================================================

Format
----------------------------------------------------------------------   

JSON output goes to standard output by default.
Each generated tag line is represented as an object.

.. code-block:: console

    $./ctags --output-format=json /tmp/foo.py
    {"_type": "tag", "name": "Foo", "path": "/tmp/foo.py", "pattern": "/^class Foo:$/", "kind": "class"}


Object keys which do not start with `_` are normal fields and map
directly to the fields of the default tags file format.

Keys that have names starting with `_` are a JSON format meta field.
Currently only `_type` is used and it can have the values `tag` for a
normal tag or `ptag` for a pseudo tag.

JSON output is still under development and it is expected the format
will change in the future. To give applications a chance to handle
these changes ctags uses a pseudo tag, `JSON_OUTPUT_VERSION`, for
specifying the format version.

.. code-block:: console

   $ ./ctags --extras='p' --pseudo-tags=JSON_OUTPUT_VERSION  --output-format=json /tmp/foo.py
   {"_type": "ptag", "name": "JSON_OUTPUT_VERSION", "path": "0.0", "pattern": "in development"}
   {"_type": "tag", "name": "Foo", "path": "/tmp/foo.py", "pattern": "/^class Foo:$/", "kind": "class"}
   ...

The JSON output format is newly designed and does not need to support
the historical quirks of the default tags file format.

Kind long names are always used instead of kind letters. Enabling the
`k` and/or `K` fields enables the `z` {kind} field internally.

Scope information is always split into scope kinds and scope names.
Enabling the `s` field enables the `Z` {kind} and `p` {scopeKind}
fields internally. As for all kinds, long names are used for printing
; kind letters are never used.

If you need kind letters, open an issue at the GitHub site of
Universal-ctags.

.. NOT REVIEWED YET

Field introspection
----------------------------------------------------------------------   

Values for the most of all fields are represented in JSON string type.
However, some of them are represented in integer type and/or boolean type.
What kind of JSON data types used in a field can be known with the output
of ``--list-fields`` option:

        $ ./ctags --list-fields
        #LETTER NAME            ENABLED LANGUAGE         XFMT   JSTYPE DESCRIPTION
        N       name            on      NONE             TRUE   s--    tag name (fixed field)
        ..
        f       file            on      NONE             TRUE   --b    File-restricted scoping
        i       inherits        off     NONE             TRUE   s-b    Inheritance information
        ...
        n       line            off     NONE             TRUE   -i-    Line number of tag definition
        ...
    
`JSTYPE` column tells the data type of fields.

s
	string

i
	integer

b
	boolean

For example, The value for "inherits" field is represented in the string or boolean type.

	
