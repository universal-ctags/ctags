# Testing PackCC

## How to run the tests

For running the tests, we assume you have `bats-core` and `uncrustify`.

If you do not have `bats-core` installed, you can do it using your package manager or from its tarball:
```
$ curl -L -o bats-core-1.2.1.tar.gz https://github.com/bats-core/bats-core/archive/v1.2.1.tar.gz &&
  tar zxvf bats-core-1.2.1.tar.gz &&
  cd bats-core-1.2.1 &&
  sudo ./install.sh /usr/local
```

If you do not have `uncrustify` installed, you can do it using your package manager or from its repository:
```
$ git clone https://github.com/uncrustify/uncrustify &&
  cd uncrustify &&
  mkdir -p build &&
  cd build &&
  cmake .. &&
  make &&
  sudo make install
```

When you use MinGW-w64, `cmake` requires the options `-G "MSYS Makefiles" -DCMAKE_INSTALL_PREFIX=/usr/local`.

After installing `bats-core-1.2.1` and `uncrustify`, you can run the tests using `tests/test.sh` script:
```
$ ./test.sh
 ✓ Testing basic.d - generation
 ✓ Testing basic.d - compilation
 ✓ Testing basic.d - run
 ...
 ✓ Testing strings.d - generation
 ✓ Testing strings.d - compilation
 ✓ Testing strings.d - run
 ✓ Testing strings.d - run [utf8]

19 tests, 0 failures, 1 skipped
```

The script passes all its arguments to `bats`, for example to run only some tests,
you can call it with `--filter <regexp>`. To see all the available arguments, execute `tests/test.sh --help`.

The behavior of the `test.sh` can also be influenced by environment variables:
 - `PACKCC` - Path to a compiled `packcc` binary. If unset, the script will compile it before running the tests.
 - `CC` - Compiler to use to compile `packcc` (if necessary) and the programs for testing. Defaults to `cc`.

## How to write a generic test

To create a new test, just follow these simple steps:

1. Create a directory with suitable name, e.g.: `tests/sequence.d`.
2. Create a grammar file called `input.peg` in this directory.
3. Create one or more input files. The files must match the glob pattern `input*.txt`.
4. Create a file with expected output for each of the inputs. The names must match the input,
   just replace "input" with "expected". E.g.: for `input-1.txt`, there must be `expected-1.txt`.

Each test automatically performs three or more test cases:

1. Generates a parser from the `input.peg` grammar.
2. Compiles the generated parser.
3. Runs the parser with specified inputs, comparing the outputs with the contents of the respective expected files.

## How to write a customized test

Sometimes the auto-generated test is not exactly what you need. In this case, you can simply create a customized test on your own:

1. Create a directory with a suitable name, e.g.: `tests/sequence.d`.
2. Create one or more `*.bats` files in this directory.
3. Specify a custom test in the bats file.

The test script will notice the customized files and will not generate a generic one.
However, you can still reuse as much of the common code as you want simply by loading `tests/utils.sh`
and calling the prepared functions. See [calc.d/calc.bats](calc.d/calc.bats) as an example.

## How to skip a test input

*Note: This paragraph applies only to automatically generated tests. For customized tests,
just add `skip` directive to your* `*.bats` *file as needed.*

Sometimes it is useful to skip a test input, for example to avoid an input that triggers a known bug
that has not yet been fixed. To do so, simply rename the input file to `input*.skip.txt`.

If you want to skip all test inputs in the directory, rename the grammar file to `input.skip.peg`.
