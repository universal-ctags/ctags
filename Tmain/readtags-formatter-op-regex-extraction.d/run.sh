#!/bin/sh

# Copyright: 2023 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
    skip "no readtags"
fi

echo '#' /GROUP/
${V} ${READTAGS} -t output.tags \
	 -Q '(eq? $scope "enum:bpf_prog_type")' \
	 -S '(<> $nth &nth)' \
	 -F '(list "[" $nth "] = \"" (downcase (#/BPF_PROG_TYPE_(.*)/ $name 1)) "\",\n")' \
	 -l

echo '#' /group/i
${V} ${READTAGS} -t output.tags \
	 -Q '(eq? $scope "enum:bpf_prog_type")' \
	 -S '(<> $nth &nth)' \
	 -F '(list "[" $nth "] = \"" (#/bpf_prog_type_(.*)/i $name 1) "\",\n")' \
	 -l

echo '#' /GROUP/ with a fallback value
${V} ${READTAGS} -t output.tags \
	 -Q '(eq? $scope "enum:bpf_prog_type")' \
	 -S '(<> $nth &nth)' \
	 -F '(list "[" $nth "] = \"S" (#/BPF_PROG_TYPE_S(.*)/ $name 1 "???") "\",\n")' \
	 -l

echo '#' /GROUP/i with a fallback value
${V} ${READTAGS} -t output.tags \
	 -Q '(eq? $scope "enum:bpf_prog_type")' \
	 -S '(<> $nth &nth)' \
	 -F '(list "[" $nth "] = \"s" (downcase (#/bpf_prog_type_s(.*)/i $name 1 "???")) "\",\n")' \
	 -l

echo '#' /GROUP = 0/
${V} ${READTAGS} -t output.tags \
	 -Q '(eq? $scope "enum:bpf_prog_type")' \
	 -S '(<> $nth &nth)' \
	 -F '(list "[" $nth "] = \"" (downcase (#/BPF_PROG_TYPE_(.*)/ $name 0)) "\",\n")' \
	 -l

echo '#' /GROUP = empty/
${V} ${READTAGS} -t output.tags \
	 -Q '(eq? $scope "enum:bpf_prog_type")' \
	 -S '(<> $nth &nth)' \
	 -F '(list "[" $nth "] = \"" (downcase (#/BPF_PROG_TYPE_UNSPE(.*)()/ $name 2)) "\",\n")' \
	 -l

echo '#' /GROUP = 100/
${V} ${READTAGS} -t output.tags \
	 -Q '(eq? $scope "enum:bpf_prog_type")' \
	 -S '(<> $nth &nth)' \
	 -F '(list "[" $nth "] = \"" (downcase (#/BPF_PROG_TYPE_UNSPE(.*)/ $name 100)) "\",\n")' \
	 -l
