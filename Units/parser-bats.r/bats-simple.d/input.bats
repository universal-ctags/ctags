#!/usr/bin/env bats

@test "Zeroth test" {
  # will have no tags
}

# bats file_tags=a:b
# bats test_tags=c:d

@test "First test" {
  # will be tagged a:b, c:d
}

# bats file_tags=

@test "Second test" {
  # will have no tags
}

@test "Loading" {
      load "ABC"
      load "a\"b"
      load "c\\d"
      load EFG
      load 'HIJ'
      load 'h\ij'
      load K\ L\;M
}

# Taken from https://bats-core.readthedocs.io/en/stable/writing-tests.html
