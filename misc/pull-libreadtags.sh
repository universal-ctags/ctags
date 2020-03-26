#!/bin/sh

# Update to the latest packcc
#
# Copyright (C) 2020 Universal Ctags Team
# License: GPL 2 or later

cd $(git rev-parse --show-toplevel)
git subtree pull --prefix libreadtags https://github.com/universal-ctags/libreadtags.git master --squash
