#!/usr/bin/env bats

load "$TESTDIR/utils.sh"

@test "Testing code_generation.d - generation" {
    test_generate
}

@test "Testing code_generation.d - compilation" {
    test_compile \
        -D pcc_create=my_create \
        -D pcc_destroy=my_destroy \
        -D pcc_parse=my_parse \
        -D pcc_context_t=my_context_t \
        -D RET_TYPE=double
}

@test "Testing code_generation.d - earlyheader" {
    in_header "EARLY HEADER ONLY"
    ! in_source "EARLY HEADER ONLY"
}

@test "Testing code_generation.d - earlycommon" {
    in_header "EARLY HEADER AND SOURCE"
    in_source "EARLY HEADER AND SOURCE"
}

@test "Testing code_generation.d - earlysource" {
    ! in_header "EARLY SOURCE ONLY"
    in_source "EARLY SOURCE ONLY"
}

@test "Testing code_generation.d - header" {
    in_header "HEADER ONLY"
    ! in_source "HEADER ONLY"
}

@test "Testing code_generation.d - common" {
    in_header "HEADER AND SOURCE"
    in_source "HEADER AND SOURCE"
}

@test "Testing code_generation.d - source" {
    ! in_header "custom_function"
    in_source "custom_function"
}

@test "Testing code_generation.d - post-source" {
    ! in_header "SOURCE AFTER GENERATED CODE"
    in_source "SOURCE AFTER GENERATED CODE"
}

@test "Testing code_generation.d - value" {
    in_header "int my_parse(my_context_t *ctx, double *ret)"
    in_source "int my_parse(my_context_t *ctx, double *ret)"
}

@test "Testing code_generation.d - auxil" {
    in_header "my_context_t *my_create(long auxil)"
    in_source "my_context_t *my_create(long auxil)"
    in_source "typedef long pcc_auxil_t"
}

@test "Testing code_generation.d - prefix" {
    in_header "typedef struct my_context_tag my_context_t"
    in_header "my_context_t *my_create(long auxil)"
    in_header "int my_parse(my_context_t *ctx, double *ret)"
    in_header "void my_destroy(my_context_t *ctx)"

    in_source "my_context_t *my_create(long auxil)"
    in_source "int my_parse(my_context_t *ctx, double *ret)"
    in_source "void my_destroy(my_context_t *ctx)"

    ! in_header "pcc_"
    ! in_source "pcc_context_t"
    ! in_source "pcc_create"
    ! in_source "pcc_parse"
    ! in_source "pcc_destroy"
}

@test "Testing code_generation.d - header ordering" {
    EARLYHEADER=$(get_line "EARLY HEADER ONLY" parser.h)
    EARLYCOMMON=$(get_line "EARLY HEADER AND SOURCE" parser.h)
    IFNDEF=$(get_line "#ifndef PCC_INCLUDED_PARSER_H" parser.h)
    HEADER=$(get_line "HEADER ONLY" parser.h)
    COMMON=$(get_line "HEADER AND SOURCE" parser.h)
    API=$(get_line "my_create" parser.h)

    [ "$EARLYHEADER" -lt "$EARLYCOMMON" ]
    [ "$EARLYCOMMON" -lt "$IFNDEF" ]
    [ "$HEADER" -lt "$COMMON" ]
    [ "$COMMON" -lt "$API" ]
}

@test "Testing code_generation.d - source ordering" {
    EARLYSOURCE=$(get_line "EARLY SOURCE ONLY" parser.c)
    EARLYCOMMON=$(get_line "EARLY HEADER AND SOURCE" parser.c)
    STDINCLUDE=$(get_line '#include <stdio.h>' parser.c)
    INCLUDE=$(get_line '#include "parser.h"' parser.c)
    SOURCE=$(get_line "custom_function" parser.c)
    COMMON=$(get_line "HEADER AND SOURCE" parser.c)
    GENERATED_START=$(get_line "#define PCC_BUFFERSIZE" parser.c)
    GENERATED_END=$(get_line "my_destroy" parser.c)
    POST_SOURCE=$(get_line "SOURCE AFTER GENERATED CODE" parser.c)

    [ "$EARLYSOURCE" -lt "$EARLYCOMMON" ]
    [ "$EARLYCOMMON" -lt "$STDINCLUDE" ]
    [ "$INCLUDE" -lt "$SOURCE" ]
    [ "$SOURCE" -lt "$COMMON" ]
    [ "$COMMON" -lt "$GENERATED_START" ]
    [ "$GENERATED_START" -lt "$GENERATED_END" ]
    [ "$GENERATED_END" -lt "$POST_SOURCE" ]
}
