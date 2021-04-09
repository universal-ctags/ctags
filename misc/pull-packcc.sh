#!/bin/sh

# Update to the latest packcc
#
# Copyright (C) 2019 Universal Ctags Team
# License: GPL 2 or later

cd $(git rev-parse --show-toplevel)
git subtree pull --prefix misc/packcc https://github.com/arithy/packcc.git master --squash
