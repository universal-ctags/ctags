# Universal Ctags Documentation #

Go to https://docs.ctags.io to read formatted version of this documentation.

## reStructuredText (Docutils and Sphinx) ##

* [Docutils](https://docutils.sourceforge.io/docs/index.html) is used to format
`man/*.rst.in` for man pages. Only reStructuredText syntaxes described
[here](https://docutils.sourceforge.io/rst.html) can be used for the man pages.

* [Sphinx Python Documentation Generator](https://www.sphinx-doc.org/en/master/index.html) is used to format `docs/*.rst`.
See [here](https://www.sphinx-doc.org/en/master/usage/restructuredtext/index.html) for more details of reStructuredText extended by Sphinx.

## Rules for writing documents ##

See also [Writing Documents](https://docs.ctags.io/en/latest/contributions.html#writing-documents).

### reStructuredText Markup Rules

* put two grave accents around a single-word option, an option usage like
  `--langdef=MyLang`, a file path, and so on, as "``` ``--foo`` ```".  For a
  single character, add single quotes around it as "``` '``-``' ```".

* put two grave accents and double quotes around a multi-word option and an
  example of a command line, as  " ``` "``-f file_name``" ``` " or " ```
  "``ctags --help``" ``` ".

* put double quotes around referring a section, e.g. " `` See "Writing
  Documents". `` ".

* use one asterisk (emphasis) for a newly-introduced conceptual *word* as
  "`*word*`".

* use one asterisk and "`<>`" as "`*<LANG>*`" for an option parameter like
  `<LANG>` in `--kind-<LANG>` option.

* To represent a backslash, surround it with double backquote. i.e. "``` ``\`` ```".
  Escaping a backslash with another backslash doesn't work well depending
  on the tools. When converting rst to man, two backslashes are converted
  into one, however when converting to html, four backslashes are converted
  into one.

### Hyperlinks

* "`` `title`_ ``"  and "`` `string <title>`_ ``" styles are valid only in the same page.
  When this is used on `rst2man`, it is shown with underline.

* "`` :ref:`label` ``" and "`` :ref:`string <label>` ``" style can jump across files.
  `rst2html` (and sphinx) converts this to a hyperlink.
  But this cause error on `rst2man` which is used to format man pages.
  Don't use this style in man pages.

* The titles of man pages (e.g. "``ctags(1)``", "``ctags-optlib(7)``", etc.) in
  ``man/*.[1-9].rst.in`` are replaced to hyperlinks as "`` :ref:`ctags(1)` ``"
  by "``make update-docs``".

### Markers ###

- "`NOT REVIEWED YET`" means the section or block is not reviewed yet.
- "`IN MAN PAGE`" means the topic is also explained in the man page of ctags.
- "`.. TODO: ...`": TODO comments for documents.
- "`.. TODO(code): ...`": TODO comments for programming codes.
- "`.. TESTCASE: ...`": A test case for the feature documented is at ....

##  Generating man pages ###

The files in `docs/man/` directory are generated from the man pages in `man/`
directory. **Do not edit the files in `docs/man/` directory directly.**

Execute `make` in the top directory to update them. Or

```sh
make -C man
```

will build the man pages only.

During this process, hyperlinks to man pages are added as described above, and
delete unnecessary section markups from `docs/man/*.rst`.  See `man/Makefile.am`
for more details.

To generate the man pages `rst2man` command is needed.
`rst2man` is part of the `python-docutils` package on Ubuntu.
