# Universal Ctags Documentation #

For easy reading of this documentation go to https://docs.ctags.io

## Markers ##

"NOT REVIEWED YET" means the section or block is not reviewed yet.

"IN MAN PAGE" means the topic is also explained in the man page of ctags.

## Rules for writing documents ##

To represent a backslash, surround it with double backquote. i.e. ``` ``\`` ```.
Escaping a backslash with another backslash doesn't work well depending
on the tools. When converting rst to man, two backslashes are converted
into one, however when converting to html, four backslashes are converted
into one.

##  Generating man pages ##

The files in `man/` directory are generated from the man pages in `../man/`
directory. **Do not edit the files in `man/` directory directly.**

Execute the following command in the top directory to update them:

```sh
make -C man QUICK=1 update-docs
```

To genereate the man pages rst2man command is needed.
rst2man is part of the python-docutils package on Ubuntu.
