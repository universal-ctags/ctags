# $Id$
# Makefile for Exuberant Ctags on RISC OS

GCC   = gcc -DRISCOS -mthrowback
REGEX = -I RegEx:
OPT   = -O3
CC    = $(GCC) $(OPT)

# Object list
OBJECTS = \
	o.args o.asm o.asp o.awk o.eiffel o.beta o.clang o.cobol o.entry \
	o.fortran o.get o.keyword o.lisp o.lregex o.lua o.main o.make \
	o.options o.parse o.pascal o.perl o.php o.python o.read o.rexx \
	o.routines o.ruby o.scheme o.sh o.slang o.sort o.strlist o.tcl \
	o.verilog o.vim o.vstring o.yacc

all: $(OBJECTS)
	gcc -o ctags $(OBJECTS) RegEx:libregex

install: ctags
	squeeze -v ctags ctags
clean:
	create o.!fake! 0
	wipe o.* ~cf
	IfThere ctags Then remove ctags

# Rules for object files

o.args:		c.args
	$(CC) -c c.args -o o.args

o.asm:		c.asm
	$(CC) $(REGEX) -c c.asm -o o.asm

o.asp:		c.asp
	$(CC) -c c.asp -o o.asp

o.awk:		c.awk
	$(CC) -c c.awk -o o.awk

o.eiffel:	c.eiffel
	$(CC) -c c.eiffel -o o.eiffel

o.beta:		c.beta
	$(CC) -c c.beta -o o.beta

o.clang:	c.c
	$(CC) -c c.c -o o.clang

o.cobol:	c.cobol
	$(CC) -c c.cobol -o o.cobol

o.entry:	c.entry
	$(CC) -c c.entry -o o.entry

o.fortran:	c.fortran
	$(CC) -c c.fortran -o o.fortran

o.get:		c.get
	$(CC) -c c.get -o o.get

o.keyword:	c.keyword
	$(CC) -c c.keyword -o o.keyword

o.lisp:		c.lisp
	$(CC) -c c.lisp -o o.lisp

o.lregex:	c.lregex
	$(CC) $(REGEX) -c c.lregex -o o.lregex

o.lua:		c.lua
	$(CC) -c c.lua -o o.lua

o.main:		c.main
	$(CC) -c c.main -o o.main

o.make:		c.make
	$(CC) -c c.make -o o.make

o.options:	c.options
	$(CC) -c c.options -o o.options

o.parse:	c.parse
	$(CC) -c c.parse -o o.parse

o.pascal:	c.pascal
	$(CC) -c c.pascal -o o.pascal

o.perl:		c.perl
	$(CC) -c c.perl -o o.perl

o.php:		c.php
	$(CC) -c c.php -o o.php

o.python:	c.python
	$(CC) -c c.python -o o.python

o.read:		c.read
	$(CC) -c c.read -o o.read

o.rexx:		c.rexx
	$(CC) -c c.rexx -o o.rexx

o.routines:	c.routines
	$(CC) -c c.routines -o o.routines

o.ruby:		c.ruby
	$(CC) -c c.ruby -o o.ruby

o.scheme:	c.scheme
	$(CC) -c c.scheme -o o.scheme

o.sh:		c.sh
	$(CC) -c c.sh -o o.sh

o.slang:	c.slang
	$(CC) -c c.slang -o o.slang

o.sort:		c.sort
	$(CC) -c c.sort -o o.sort

o.sml:		c.sml
	$(CC) -c c.sml -o o.sml

o.sql:		c.sql
	$(CC) -c c.sql -o o.sql

o.strlist:	c.strlist
	$(CC) -c c.strlist -o o.strlist

o.tcl:		c.tcl
	$(CC) -c c.tcl -o o.tcl

o.vim:		c.vim
	$(CC) -c c.vim -o o.vim

o.vstring:	c.vstring
	$(CC) -c c.vstring -o o.vstring

o.yacc:		c.yacc
	$(CC) -c c.yacc -o o.yacc

# End of Makefile
