.. _ctags-json-output(5):

==============================================================
ctags-json-output
==============================================================

JSON based ctags output

:Version: 0.0
:Manual group: Universal Ctags
:Manual section: 5

SYNOPSIS
--------
|	**ctags** --output-format=json ...

DESCRIPTION
-----------
Universal Ctags supports `JSON <https://www.json.org/>`_ (strictly
speaking `JSON Lines <https://jsonlines.org/>`_) output format if the
ctags executable is built with ``libjansson``.  JSON output goes to
standard output by default.

Format
------
Each JSON line represents a tag.

.. code-block:: console

	$ ctags --extras=+p --output-format=json --fields=-s input.py
	{"_type": "ptag", "name": "JSON_OUTPUT_VERSION", "path": "0.0", "pattern": "in development"}
	{"_type": "ptag", "name": "TAG_FILE_SORTED", "path": "1", "pattern": "0=unsorted, 1=sorted, 2=foldcase"}
	...
	{"_type": "tag", "name": "Klass", "path": "/tmp/input.py", "pattern": "/^class Klass:$/", "language": "Python", "kind": "class"}
	{"_type": "tag", "name": "method", "path": "/tmp/input.py", "pattern": "/^    def method(self):$/", "language": "Python", "kind": "member", "scope": "Klass", "scopeKind": "class"}
	...

A key not starting with ``_`` is mapped to a field of ctags.
"``--output-format=json --list-fields``" options list the fields.

A key starting with ``_`` represents meta information of the JSON
line.  Currently only ``_type`` key is used. If the value for the key
is ``tag``, the JSON line represents a regular tag. If the value is
``ptag``, the line represents a pseudo-tag.

The output format can be changed in the
future. ``JSON_OUTPUT_VERSION`` pseudo-tag provides a change
client-tools to handle the changes.  Current version is "0.0". A
client-tool can extract the version with ``path`` key from the
pseudo-tag.

The JSON output format is newly designed and has no limitation found
in the default tags file format.

* The values for ``kind`` key are represented in long-name flags.
  No one-letter is here.

* Scope names and scope kinds have distinguished keys: ``scope`` and ``scopeKind``.
  They are combined in the default tags file format.

Data type used in a field
-------------------------
Values for the most of all keys are represented in JSON string type.
However, some of them are represented in string, integer, and/or boolean type.

"``--output-format=json --list-fields``" options show What kind of data type
used in a field of JSON.

.. code-block:: console

	$ ctags --output-format=json --list-fields
	#LETTER NAME           ENABLED LANGUAGE         JSTYPE FIXED DESCRIPTION
	F       input          yes     NONE             s--    no    input file
	...
	P       pattern        yes     NONE             s-b    no    pattern
	...
	f       file           yes     NONE             --b    no    File-restricted scoping
	...
	e       end            no      NONE             -i-    no    end lines of various items
	...

``JSTYPE`` column shows the data types.

'``s``'
	string

'``i``'
	integer

'``b``'
	boolean (true or false)

For an example, the value for ``pattern`` field of ctags takes a string or a boolean value.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`tags(5) <tags(5)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`
