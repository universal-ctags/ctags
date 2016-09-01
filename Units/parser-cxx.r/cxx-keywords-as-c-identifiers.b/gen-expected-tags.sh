#!/bin/sh

# Copyright: 2016 Masatake YAMATO
# License: GPL-2

. ./keywords.sh

gen_struct_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
$k	input.h	/^struct $k {int x;} v$i;\$/;"	s
EOF
	i=$(( i + 1 ))
    done
}

gen_member_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
$k	input.h	/^struct s$i {int $k;} ;\$/;"	m	struct:s$i	typeref:typename:int
EOF
	i=$(( i + 1 ))
    done
}

gen_typedef_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
$k	input.h	/^typedef int $k; \/* $i *\/\$/;"	t	typeref:typename:int
EOF
	i=$(( i + 1 ))
    done
}

gen_var_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
$k	input.h	/^int v$i, $k;\$/;"	v	typeref:typename:int
EOF
	i=$(( i + 1 ))
    done
}

gen_func_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
$k	input.h	/^int $k (int a$i);\$/;"	p	typeref:typename:int
EOF
	i=$(( i + 1 ))
    done
}

gen_struct_tags
gen_member_tags
gen_typedef_tags
gen_var_tags
gen_func_tags

