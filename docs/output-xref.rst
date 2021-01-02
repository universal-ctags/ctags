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
