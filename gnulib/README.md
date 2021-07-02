# README for `gnulib/` directory

The files in this directory were taken from [the GNU Portability Library
(Gnulib)](https://www.gnu.org/software/gnulib/).

See "1. Bread Overview" and "3. Invoking gnulib-tool" in [the GNU Gnulib
documents](https://www.gnu.org/software/gnulib/manual/gnulib.html) for more details.

## Checking out Gnulib

```
$ git clone git://git.sv.gnu.org/gnulib.git
```

In the following description, the directory where Gnulib is checked out is
called `$GNULIB_DIR`. And the root directory of the Universal Ctags is called
`$CTAGS_DIR`.

## Importing the first module

The first module (`regex`) was imported by the following command into the
directory `gnulib/` without modifying `.gitignore`.

```
$ cd $CTAGS_DIR
$ gnulib-tool --source-base=gnulib --no-vc-files --import regex
```

The options used are saved in `m4/gnulib-cache.m4`.

## Updating the modules

`-add-import` without module names updates imported modules.

```
$ cd $GNULIB_DIR
$ git pull
$ cd $CTAGS_DIR
$ gnulib-tool --add-import
```

## Importing additional modules

```
$ gnulib-tool --add-import foo bar ...
```
