# Universal Ctags Documentation #

Go to https://docs.ctags.io to read formatted version of this documentation.

## reStructuredText and Sphinx ##

[Sphinx Python Documentation Generator](https://www.sphinx-doc.org/en/master/index.html) is used to format this documents.
See [here](https://www.sphinx-doc.org/en/master/usage/restructuredtext/index.html) for more details of reStructuredText extended by Sphinx.

## Rules for writing documents ##

### Markup Rules

* put two single quotes around an option name like ``` ``--help`` ```.

* put two single quotes with colon around field name like ``` ``field:`` ```.

* put double quotes aground an example of command line, option usage like
  "--langdef=MyLang", referring a section, and a file path.

* use two asterisks for newly introduced conceptual `**word**` and reserved
  words. The reserved words are

  * "default" in --langmap

* use one asterisk for option parameter like `*LANG*` in --kind-<LANG> option.

* To represent a backslash, surround it with double backquote. i.e. ``` ``\`` ```.
  Escaping a backslash with another backslash doesn't work well depending
  on the tools. When converting rst to man, two backslashes are converted
  into one, however when converting to html, four backslashes are converted
  into one.

### Hyperlinks

* `` `phrase reference`_ `` style is valid only in the same page.
When this is used on rst2man, it is shown with underline.

* rst2html (and sphinx) converts this to a hyperlink.
`` :ref:`title <tag>` `` style can jump across files, but this can
be used only on sphinx and cause error on old docutils
(rst2man).

### Markers ###

- "NOT REVIEWED YET" means the section or block is not reviewed yet.
- "IN MAN PAGE" means the topic is also explained in the man page of ctags.

### Representing a backslash ###

To represent a backslash, surround it with double backquote. i.e. ``` ``\`` ```.
Escaping a backslash with another backslash doesn't work well depending
on the tools. When converting rst to man, two backslashes are converted
into one, however when converting to html, four backslashes are converted
into one.

###  Generating man pages ###

The files in `man/` directory are generated from the man pages in `../man/`
directory. **Do not edit the files in `man/` directory directly.**

Execute the following command in the top directory to update them:

```sh
make -C man QUICK=1 update-docs
```

To generate the man pages `rst2man` command is needed.
`rst2man` is part of the python-docutils package on Ubuntu.
