#!/bin/sh

set -e
set -x

./configure --enable-debugging
make -j2
make check
