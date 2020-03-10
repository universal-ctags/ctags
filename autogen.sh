#!/bin/sh

set -xe

for c in autoconf automake libtoolize autoreconf; do
	type $c || exit 1
done

autoreconf -vfi
