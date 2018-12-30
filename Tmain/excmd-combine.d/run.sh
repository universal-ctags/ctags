#!/bin/sh

# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

$CTAGS --quiet --options=NONE -o - --excmd=combine input.cpp
