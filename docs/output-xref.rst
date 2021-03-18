.. NOT REVIEWED YET

.. _output-xref:

======================================================================
Xref output
======================================================================

Xref output is a tabular, human-readable cross reference (xref) format.

The default information contained in the output includes:

* the tag name
* the kind of tag
* the line number
* file name
* source line (with extra white space condensed) of the file

``--_xformat`` option allows a user to customize the output information.  See
:ref:`Customizing xref output <xformat>` for more details.

Xref output goes to standard output by default.

Notes:

    * Printing `z`{kind} field in xref format doesn't include `kind:` prefix.
    * Printing `Z`{scope} field in xref format doesn't include `scope:` prefix.

.. _xformat:

Customizing xref output
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``--_xformat`` option allows a user to customize the cross reference
(xref) output enabled with ``-x``.
::

   --_xformat=FORMAT


The notation for FORMAT is similar to that employed by `printf(3)` in
the C language; `%` represents a slot which is substituted with a
field value when printing. You can specify multiple slots in FORMAT.
Here field means an item listed with ``--list-fields`` option.

The notation of a slot::

   %[-][.][WIDTH-AND-ADJUSTMENT]FIELD-SPECIFIER

``FIELD-SPECIFIER`` specifies a field whose value is printed.
Short notation and long notation are available. They can be mixed
in a FORMAT. Specifying a field with either notation, one or more
fields are activated internally.

The short notation is just a letter listed in the LETTER column of
the ``--list-fields`` output.

The long notation is a name string surrounded by braces(`{` and
`}`). The name string is listed in the NAME column of the output of
the same option. To specify a field owned by a parser, prepend
the parser name to the name string with `.` as a separator.

Wild card (`*`) can be used where a parser name is specified. In this
case both common and parser specific fields are activated and printed.
If a common field and a parser specific field have the same name,
the common field has higher priority.

`WIDTH-AND-ADJUSTMENT` is a positive number.
The value of the number is used as the width of
the column where a field is printed. The printing is
right adjusted by default, and left
adjusted when `-` is given as prefix.
The output is not truncated by default even if its field width is
specified and smaller than width of output value. For truncating
the output to the specified width, use `.` as prefix.

An example of specifying common fields:

.. code-block:: console

    $  ctags -x --_xformat="%-20N %4n %-16{input}|" main/main.c | head
    CLOCKS_PER_SEC        360 main/main.c     |
    CLOCKS_PER_SEC        364 main/main.c     |
    CLOCK_AVAILABLE       358 main/main.c     |
    CLOCK_AVAILABLE       363 main/main.c     |
    Totals                 87 main/main.c     |
    __anonae81ef0f0108     87 main/main.c     |
    addTotals             100 main/main.c     |
    batchMakeTags         436 main/main.c     |
    bytes                  87 main/main.c     |
    clock                 365 main/main.c     |

Here `%-20N %4n %-16{input}|` is a format string. Let's look at the
elements of the format.

`%-20N`

	The short notation is used here.
	The element means filling the slot with the name of the tag.
	The width of the column is 20 characters and left adjusted.

`%4n`

	The short notation is used here.
	The element means filling the slot with the line number of
	the tag. The width of the column is 4 characters and right
        adjusted.

`%-16{input}`

	The long notation is used here.
	The element means filling the slot with the input file name
	where the tag is defined. The width of column is 16
        characters and left adjusted.

`|`

	Printed as is.

Another example of specifying parser specific fields:

.. code-block:: console

	$  ctags -x --_xformat="%-20N [%10{C.properties}]" main/main.c
	CLOCKS_PER_SEC       [          ]
	CLOCK_AVAILABLE      [          ]
	Totals               [          ]
	__anonae81ef0f0108   [          ]
	addTotals            [    extern]
	batchMakeTags        [    static]
	bytes                [          ]
	clock                [          ]
	clock                [    static]
	...

Here `"%-20N [%10{C.properties}]"` is a format string. Let's look at
the elements of the format.

`%-20N`

	Already explained in the first example.

`[` and `]`

	Printed as is.

`%10{C.properties}`

	The long notation is used here.
	The element means filling the slot with the value
	of the properties field of the C parser.
	The width of the column is 10 characters and right adjusted.


.. TODO: An example of using WILDCARD
