.. _ctags-lang-iPythonCell(7):

==============================================================
ctags-lang-iPythonCell
==============================================================

The man page of the iPythonCell parser for Universal Ctags

:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --extras=+{subparser} --languages=+iPythonCell,Python \\
|                     [--extras-IPythonCell=+{doubleSharps}] \\
|                     [--regex-IPythonCell=/<PATTERN>/\\n/c/] ...

DESCRIPTION
-----------
iPythonCell parser is a subparser stacked on top of the Python parser.
It works when:

* the Python parser is enabled,
* the ``subparser`` extra is enabled, and
* the iPythonCell parser itself is enabled.

The iPythonCell parser extracts cells explained as in vim-ipython-cell
(https://github.com/hanschen/vim-ipython-cell/blob/master/README.md).

KIND(S)
-------
The iPythonCell parser defines only a ``cell`` kind.

EXTRA(S)
--------

Tagging cells staring with ``##...`` is disabled by default because
the pattern is too generic; with that pattern unwanted tags can be extracted.

Enable ``doubleSharps`` language specific extra for tagging cells
staring with ``##...``.

CUSTOMIZING
-----------
If your favorite cell pattern is not supported in the parser, you can
define the pattern in your ``.ctagd.d/your.ctags`` or command lines.
Here is an example how to support "``# CTAGS: ...``":

"input.py"

.. code-block:: Python

	x=1
	# CTAGS: DEFINE F
	def F():
		# CTAGS: DO NOTING
		pass

"output.tags"
with "--options=NONE --sort=no --extras=+{subparser} --regex-IPythonCell=/[ \t]*# CTAGS:[ ]?(.*)$/\1/c/ -o - input.py"

.. code-block:: tags

   x	input.py	/^x=1$/;"	v
   DEFINE F	input.py	/^# CTAGS: DEFINE F$/;"	c
   F	input.py	/^def F():$/;"	f
   DO NOTING	input.py	/^	# CTAGS: DO NOTING$/;"	c

You can put "``--regex-IPythonCell=/[ \t]*# CTAGS:[ ]?(.*)$/\1/c/``" in ``your.ctags``
to avoid specifying the pattern repeatedly.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`, :ref:`ctags-lang-python(7) <ctags-lang-python(7)>`
