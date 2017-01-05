#!/usr/bin/env bash

#
# Taken from the original bug report (https://github.com/universal-ctags/ctags/issues/1261)
#

a::test() {
    echo "test"
}

function a::anotherTest() {
    echo "another test"
}
