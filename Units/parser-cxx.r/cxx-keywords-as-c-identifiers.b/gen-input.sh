#!/bin/sh

# Copyright: 2016 Masatake YAMATO
# License: GPL-2

. ./keywords.sh

gen_struct_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
struct $k {int x;} v$i;
EOF
	i=$(( i + 1 ))
    done
}

gen_member_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
struct s$i {int $k;} ;
EOF
	i=$(( i + 1 ))
    done
}

gen_typedef_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
typedef int $k; /* $i */
EOF
	i=$(( i + 1 ))
    done
}

gen_var_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
int v$i, $k;
EOF
	i=$(( i + 1 ))
    done
}

gen_func_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
int $k (int a$i);
EOF
	i=$(( i + 1 ))
    done
}

gen_label_tags ()
{
    i=0
    for k in $keywords; do
	cat<<EOF
void f$i (void) { goto $k; $k: return; }
EOF
	i=$(( i + 1 ))
    done
}

gen_struct_tags
gen_member_tags
gen_typedef_tags
gen_var_tags
gen_func_tags
# gen_label_tags
