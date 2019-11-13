(
Thank you for contacting us.

If you are reporting an issue with the parsing output, please fill
the following template.  As your custom CTags configuration can
affect results, please always use `--options=NONE` as the first
option when running `ctags`.

Otherwise, delete the template and write your issue from scratch.
Examples may help developers understanding your issue better.

Use GitHub web interface and markdown notation.
Using mail results broken text rendering that makes
the developers go crazy.
)

*****

The name of the parser:


The command line you used to run ctags:

```
$ ctags --options=NONE ...
```

The content of input file:

```C
/* THIS IS AN EXAMPLE */
int
main(void)
{
	...
```

The tags output you are not satisfied with:

```
!_THIS_IS_AN_EXAMPLE
mainVoid	foo.c	/^main(void)$/;"	kind:function	line:2	language:C	typeref:typename:int	signature:(void)	roles:def
...
```

The tags output you expect:

```
!_THIS_IS_AN_EXAMPLE
main	foo.c	/^main(void)$/;"	kind:function	line:2	language:C	typeref:typename:int	signature:(void)	roles:def
...
```

The version of ctags:

```
$ ctags --version
Universal Ctags 0.0.0(EXAMPLE), Copyright (C) 2015 Universal Ctags Team
Universal Ctags is derived from Exuberant Ctags.
Exuberant Ctags 5.8, Copyright (C) 1996-2009 Darren Hiebert
  Compiled: May 11 1018, 23:16:36
  URL: https://ctags.io/
  Optional compiled features: +wildcards, +regex, +iconv, +option-directory, +xpath, +json, +interactive, +sandbox, +yaml
```

How do you get ctags binary:

(
Building it locally, via GNU/Linux distribution, as BSD's package,
win32 binary taken from Universal-ctags/ctags-win32 project, macosx
binary taken from Universal-ctags/homebrew-universal-ctags project,
etc.
)
