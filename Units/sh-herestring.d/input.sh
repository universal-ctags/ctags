#!/bin/bash
# check for Bash's here-strings as they need not to be misinterpreted as
# here-documents

f1(){ :; }
cat <<<hello
f2(){ :; }
cat <<<"hello there"
f3(){ :; }
cat <<<'hello there'
f4(){ :; }
