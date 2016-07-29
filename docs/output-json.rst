.. _output-json:

======================================================================
Json output
======================================================================

Json output goes the standard output by default.
Each tag line is represented in an object.

.. code-block:: console

    $./ctags --output-format=json /tmp/foo.py
    {"_type": "tag", "name": "Foo", "path": "/tmp/foo.py", "pattern": "/^class Foo:$/", "kind": "class"}


The pair which key is not started from `_` is a normal field.
The meanings of the pairs are the same as that of tags output.

The pair which key is started from `_` is a json format own meta field.
Currently only `_type` is used.

The key `_type` takes `tag` or `ptag`. As name show, `tag` means the
object holds a tag. `ptag` means the object holds a pseudo tag.

Json output is under development. So the format will be changed in the
future. To give applications a chance to handle the change, ctags
uses a pseudo tag, `JSON_OUTPUT_VERSION` for notifying the version of the
format.

.. code-block:: console

   $ ./ctags --extra='p' --pseudo-tags=JSON_OUTPUT_VERSION  --output-format=json /tmp/foo.py
   {"_type": "ptag", "name": "JSON_OUTPUT_VERSION", "path": "0.0", "pattern": "in development"}
   {"_type": "tag", "name": "Foo", "path": "/tmp/foo.py", "pattern": "/^class Foo:$/", "kind": "class"}
   ...

Unlike tags output format, Json output is newly designed. So the odd restrictions
and behaviors of fields control in the tags output format are not in Json output.

Kind long names are used always instead of kind letters.
Enabling `k` and/or `K` fields enables `z` {kind} field internally.

Scopes information are split into scope kinds and scope names always.
Enabling `s` field enables `Z` {kind} and `p` {scopeKind} fields internally.
As same as kind, long names are used for printing scope kinds; kind
letters are not used for printing scopes information.

If you need kind letters, open an issue at the github site of
Universal ctags.

