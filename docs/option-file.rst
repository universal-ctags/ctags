.. _option_files:

Option files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.. Q: shouldn't the section about option files (preload especially) go in
	their own section somewhere else in the docs? They're not specifically
	for "Extending ctags" - they can be used for any command options that
	you want to use permanently. It's really the new language parsers using
	--regex-<LANG> and such that are about "Extending ctags", no?

An "option file" is a file in which command line options are written line
by line. ``ctags`` loads it and runs as if the options in the file were
passed through command line.

The following file is an example of an option file:

.. code-block:: ctags

	# Exclude directories that don't contain real code
	--exclude=Units
		# indentation is ignored
		--exclude=tinst-root
	--exclude=Tmain

The character `#` can be used as a start marker of a line comment.
Whitespaces at the start of lines are ignored during loading.

And it works exactly as if we had called:

.. code-block:: sh

	ctags --exclude=Units --exclude=tinst-root --exclude=Tmain

Order of loading option files
......................................................................

Option files are loaded by ``ctags`` automatically at start-up time.

Which files are loaded at start-up time are very different from Exuberant Ctags.
See :ref:`option-file_difference` for the differences and their intentions.

At start-up time, ``ctags`` loads files having :file:`.ctags` as a
file extension under the following statically defined directories:

#. :file:`$XDG_CONFIG_HOME/ctags/`, or :file:`$HOME/.config/ctags/` if :file:`$XDG_CONFIG_HOME` is not defined (on other than Windows)
#. :file:`$HOME/.ctags.d/`
#. :file:`$HOMEDRIVE$HOMEPATH/ctags.d/` (on Windows)
#. :file:`./.ctags.d/`
#. :file:`./ctags.d/`

``ctags`` visits the directories in the order listed above for preloading files.
``ctags`` loads files having :file:`.ctags` as file extension in alphabetical
order (``strcmp(3)`` is used for comparing, so for example
:file:`.ctags.d/ZZZ.ctags` will be loaded *before* :file:`.ctags.d/aaa.ctags` in an ordinary locale).

If a option file includes ``--options=PATHNAME`` option, specified files are
loaded immediately as described in the next section. ``ctags`` load a option
file only once if it is specified multiple times.

Finally if ``--options=PATHNAME`` option is specified on ``ctags`` command line,
option files specified are load.

``--options=PATHNAME`` option
......................................................................
Exuberant Ctags also has the ``--options`` option, but you can only specify a
single file to load. Universal Ctags extends the option in two aspects:

- You can specify a directory, to load all the files in that directory.
- You can specify a PATH list to look in. See next section for details.

Specifying a directory
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

If you specify a directory instead of a file as the argument for the
``--options=PATHNAME``, ``ctags`` will load all files having a
:file:`.ctags` extension under said directory in alphabetical order.

Specifying an optlib PATH list
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Much like a command line shell, ``ctags`` has an *optlib PATH list* in which it
can look for a file (or directory) to load.

When loading a file (or directory) specified with ``--options=PATHNAME``,
ctags first checks if ``PATHNAME`` is an absolute path or a relative path.
An absolute path starts with '``/``' or '``.``'.
If ``PATHNAME`` is an absolute path, ctags tries to load it immediately.

If, on the contrary, is a relative path, ``ctags`` does two things: First,
looks for the file (or directory) in *optlib PATH list* and tries to load it.

If the file doesn't exist in the PATH list, ``ctags``  treats ``PATHNAME`` as a
path relative to the working directory and loads the file.

By default, *optlib PATH list* is empty. To set or add a directory
path to the list, use ``--optlib-dir=PATH``.

For setting (adding one after clearing)::

	--optlib-dir=PATH

For adding on the beginning of the PATH list::

	--optlib-dir=+PATH

Tips for writing an option file
......................................................................

* Use ``--quiet --options=NONE`` to disable preloading.

* ``--_echo=MSG`` and  ``--_force-quit=[NUM]`` options are introduced for
  debugging the process of loading option files. See "OPTIONS"
  section of :ref:`ctags-optlib(7) <ctags-optlib(7)>`.

* Universal Ctags has an ``optlib2c`` script that translates an option file
  into C source code. Your optlib parser can thus easily become a built-in parser.
  See :ref:`optlib2c` for details.

.. _option-file_difference:

Difference from Exuberant Ctags
......................................................................
Quoted from man page of Exuberant Ctags:

	FILES
		- /ctags.cnf (on MSDOS, MSWindows only)
		- /etc/ctags.conf
		- /usr/local/etc/ctags.conf
		- $HOME/.ctags
		- $HOME/ctags.cnf (on MSDOS, MSWindows only)
		- .ctags
		- ctags.cnf (on MSDOS, MSWindows only)

	If any of these configuration files exist, each will
	be expected to contain a set of default options
	which are read in the order listed when ctags
	starts, but before the CTAGS environment variable is
	read or any command line options are read.  This
	makes it possible to set up site-wide, personal or
	project-level defaults. It is possible to compile
	ctags to read an additional configuration file
	before any of those shown above, which will be
	indicated if the output produced by the --version
	option lists the "custom-conf" feature. Options
	appearing in the CTAGS environment variable or on
	the command line will override options specified in
	these files. Only options will be read from these
	files.  Note that the option files are read in
	line-oriented mode in which spaces are significant
	(since shell quoting is not possible). Each line of
	the file is read as one command line parameter (as
	if it were quoted with single quotes). Therefore,
	use new lines to indicate separate command-line
	arguments.

What follows explains the differences and their intentions...


Directory oriented configuration management
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Exuberant Ctags provides a way to customize ctags with options like
``--langdef=<LANG>`` and ``--regex-<LANG>``. These options are
powerful and make ctags popular for programmers.

Universal Ctags extends this idea; we have added new options for
defining a parser, and have extended existing options. Defining
a new parser with the options is more than "customizing" in
Universal Ctags.

To make easier the maintenance a parser defined using the options, you can put
each language parser in a different options file. Universal Ctags doesn't
preload a single file. Instead, Universal Ctags loads all the files having the
:file:`.ctags` extension under the previously specified directories. If you
have multiple parser definitions, put them in different files.

Avoiding option incompatibility issues
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

The Universal Ctags options are different from those of Exuberant Ctags,
therefore Universal Ctags doesn't load any of the files Exuberant Ctags loads at
start-up. Otherwise there would be incompatibility issues if Exuberant Ctags
loaded an option file that used a newly introduced option in Universal Ctags,
and vice versa.

No system wide configuration
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

To make the preload path list short and because it was rarely ever used,
Universal Ctags does not load any option files for system wide configuration.
(i.e., no :file:`/etc/ctags.d`)

Using :file:`.ctags` for the file extension
,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

Extensions :file:`.cnf` and :file:`.conf` are obsolete.
Use the unified extension :file:`.ctags` only.
