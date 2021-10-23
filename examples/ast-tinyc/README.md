# AST Builder for Tiny-C #

## Overview ##

This example builds an AST (abstract syntax tree) from an input source file
written in [Tiny-C](http://www.iro.umontreal.ca/~felipe/IFT2030-Automne2002/Complements/tinyc.c) with some extension shown below,
and prints the AST in the standard output.
If there are syntax errors, it shows them with line and column numbers.

__Extension:__
- Supports all arithmetic, logical and bitwise operators,
- Accepts octal and hexadecimal integers,
- Permits uppercase letters, number letters, and underscores in an identifier,
- Accepts `'\v'` and `'\f'` as white-spaces,
- Supports comment blocks.

This example is placed in the public domain!
So, you can use it freely without noticing the copyright of the original author.

## How to compile this example ##

### For Unix-like OS ###

You can get the executable by executing the following commands:

```
cd /path/to/this_directory
mkdir build
cd build
cmake -DPACKCC=/path/to/packcc ..
make
```

Here, `/path/to/this_directory` represents the path name of this directory,
and `/path/to/packcc` represents the path name of `packcc` command.
If `packcc` command is installed in one of the directories specified in the environment variable `PATH`,
the option `-DPACKCC=/path/to/packcc` is not necessary.

The executable `ast` will be created in the directory `build`.

### For Windows ###

#### Using Visual Studio ####

You must have [Build Tools for Visual Studio](https://visualstudio.microsoft.com/downloads/#build-tools-for-visual-studio-2019) installed in your system.
You can get the executable by executing the following commands using 'Developer Command Prompt for VS 2019' or 'Developer PowerShell for VS 2019':

```
cd /path/to/this_directory
mkdir build
cd build
cmake -DPACKCC=/path/to/packcc ..
MSBuild ALL_BUILD.vcxproj
```

Here, `/path/to/this_directory` represents the path name of this directory,
and `/path/to/packcc` represents the path name of `packcc` command.
If `packcc` command is installed in one of the directories specified in the environment variable `PATH`,
the option `-DPACKCC=/path/to/packcc` is not necessary.

The executable `ast.exe` will be created in the directory `build`.

#### Using MinGW-w64 ####

You can get the executable by executing the following commands:

```
cd /path/to/this_directory
mkdir build
cd build
cmake -G "MSYS Makefiles" -DPACKCC=/path/to/packcc ..
make
```

Here, `/path/to/this_directory` represents the path name of this directory,
and `/path/to/packcc` represents the path name of `packcc` command.
If `packcc` command is installed in one of the directories specified in the environment variable `PATH`,
the option `-DPACKCC=/path/to/packcc` is not necessary.

The executable `ast.exe` will be created in the directory `build`.

## How to run this example ##

Example input source files are prepared in [`inputs`](inputs) directory.
Most of these are taken from [grammars-v4](https://github.com/antlr/grammars-v4/) repository of [ANTLR project](https://github.com/antlr/) with thanks.

Here, it is assumed that you are in the directory `build`.
If you want to print out the AST of the input source file [`inputs/example2.c`](inputs/example2.c), execute the following command:

```
./ast ../inputs/example2.c
```

You will see the output below:

```
STATEMENT_LIST: arity = 1
  STATEMENT_LIST: arity = 3
    OPERATOR_ASSIGN: arity = 2
      IDENTIFIER: line = 2, column = 3, value = 'i'
      INTEGER_DEC: line = 2, column = 5, value = '125'
    OPERATOR_ASSIGN: arity = 2
      IDENTIFIER: line = 2, column = 10, value = 'j'
      INTEGER_DEC: line = 2, column = 12, value = '100'
    STATEMENT_WHILE: arity = 2
      OPERATOR_SUB: arity = 2
        IDENTIFIER: line = 2, column = 24, value = 'i'
        IDENTIFIER: line = 2, column = 26, value = 'j'
      STATEMENT_IF_ELSE: arity = 3
        OPERATOR_LT: arity = 2
          IDENTIFIER: line = 2, column = 33, value = 'i'
          IDENTIFIER: line = 2, column = 35, value = 'j'
        OPERATOR_ASSIGN: arity = 2
          IDENTIFIER: line = 2, column = 38, value = 'j'
          OPERATOR_SUB: arity = 2
            IDENTIFIER: line = 2, column = 40, value = 'j'
            IDENTIFIER: line = 2, column = 42, value = 'i'
        OPERATOR_ASSIGN: arity = 2
          IDENTIFIER: line = 2, column = 50, value = 'i'
          OPERATOR_SUB: arity = 2
            IDENTIFIER: line = 2, column = 52, value = 'i'
            IDENTIFIER: line = 2, column = 54, value = 'j'
```

If there are errors in the input source file, this example reports up to 4 errors at a time.
You can see this behavior by the following command using [`inputs/erroneous1.c`](inputs/erroneous1.c):

```
./ast ../inputs/erroneous1.c
```

You will see the error messages below:

```
ERROR: line 6, column 5: Unexpected token 'do'
ERROR: line 7, column 5: Statement missing after 'do'
ERROR: line 4, column 5: 'else' without corresponding 'if'
ERROR: line 3, column 5: 'else' without corresponding 'if'
```

Note that the multiple error recognition of this example is not sophisticated.
To improve it, you have to design elaborate grammar rules for error skipping to detect next multiple errors naturally.
