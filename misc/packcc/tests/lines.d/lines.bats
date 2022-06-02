#!/usr/bin/env bats

load "$TESTDIR/utils.sh"

@test "Testing lines.d - generation" {
    PACKCC_OPTS=("--lines")
    test_generate
}

@test "Testing lines.d - header" {
    in_header ":EARLYHEADER:"
    in_header ":EARLYCOMMON:"
    in_header ":HEADER:"
    in_header ":COMMON:"

    ! in_header ":EARLYSOURCE:"
    ! in_header ":SOURCE:"
    ! in_header ":CODE:"
    ! in_header ":RULE1:"
    ! in_header ":RULE2:"
    ! in_header ":RULE3:"

    EARLYHEADER_I=$(get_line ":EARLYHEADER:" input.peg)
    EARLYHEADER_O=$(get_line ":EARLYHEADER:" parser.h)
    EARLYHEADER_S=$(get_line "^#line $EARLYHEADER_I \"input.peg\"" parser.h)
    EARLYHEADER_E=$(get_line "^#line $(($EARLYHEADER_O + 1)) \"parser.h\"" parser.h)
    [ "$EARLYHEADER_S" -eq $((EARLYHEADER_O - 1)) ]
    [ "$EARLYHEADER_E" -eq $((EARLYHEADER_O + 1)) ]

    EARLYCOMMON_I=$(get_line ":EARLYCOMMON:" input.peg)
    EARLYCOMMON_O=$(get_line ":EARLYCOMMON:" parser.h)
    EARLYCOMMON_S=$(get_line "^#line $EARLYCOMMON_I \"input.peg\"" parser.h)
    EARLYCOMMON_E=$(get_line "^#line $(($EARLYCOMMON_O + 1)) \"parser.h\"" parser.h)
    [ "$EARLYCOMMON_S" -eq $((EARLYCOMMON_O - 1)) ]
    [ "$EARLYCOMMON_E" -eq $((EARLYCOMMON_O + 1)) ]

    HEADER_I=$(get_line ":HEADER:" input.peg)
    HEADER_O=$(get_line ":HEADER:" parser.h)
    HEADER_S=$(get_line "^#line $HEADER_I \"input.peg\"" parser.h)
    HEADER_E=$(get_line "^#line $(($HEADER_O + 1)) \"parser.h\"" parser.h)
    [ "$HEADER_S" -eq $((HEADER_O - 1)) ]
    [ "$HEADER_E" -eq $((HEADER_O + 1)) ]

    COMMON_I=$(get_line ":COMMON:" input.peg)
    COMMON_O=$(get_line ":COMMON:" parser.h)
    COMMON_S=$(get_line "^#line $COMMON_I \"input.peg\"" parser.h)
    COMMON_E=$(get_line "^#line $(($COMMON_O + 1)) \"parser.h\"" parser.h)
    [ "$COMMON_S" -eq $((COMMON_O - 1)) ]
    [ "$COMMON_E" -eq $((COMMON_O + 1)) ]
}

@test "Testing lines.d - source" {
    in_source ":EARLYSOURCE:"
    in_source ":EARLYCOMMON:"
    in_source ":SOURCE:"
    in_source ":COMMON:"
    in_source ":CODE:"
    in_source ":RULE1:"
    in_source ":RULE2:"
    in_source ":RULE3:"

    ! in_source ":EARLYHEADER:"
    ! in_source ":HEADER:"

    EARLYSOURCE_I=$(get_line ":EARLYSOURCE:" input.peg)
    EARLYSOURCE_O=$(get_line ":EARLYSOURCE:" parser.c)
    EARLYSOURCE_S=$(get_line "^#line $EARLYSOURCE_I \"input.peg\"" parser.c)
    EARLYSOURCE_E=$(get_line "^#line $(($EARLYSOURCE_O + 1)) \"parser.c\"" parser.c)
    [ "$EARLYSOURCE_S" -eq $((EARLYSOURCE_O - 1)) ]
    [ "$EARLYSOURCE_E" -eq $((EARLYSOURCE_O + 1)) ]

    EARLYCOMMON_I=$(get_line ":EARLYCOMMON:" input.peg)
    EARLYCOMMON_O=$(get_line ":EARLYCOMMON:" parser.c)
    EARLYCOMMON_S=$(get_line "^#line $EARLYCOMMON_I \"input.peg\"" parser.c)
    EARLYCOMMON_E=$(get_line "^#line $(($EARLYCOMMON_O + 1)) \"parser.c\"" parser.c)
    [ "$EARLYCOMMON_S" -eq $((EARLYCOMMON_O - 1)) ]
    [ "$EARLYCOMMON_E" -eq $((EARLYCOMMON_O + 1)) ]

    SOURCE_I=$(get_line ":SOURCE:" input.peg)
    SOURCE_O=$(get_line ":SOURCE:" parser.c)
    SOURCE_S=$(get_line "^#line $SOURCE_I \"input.peg\"" parser.c)
    SOURCE_E=$(get_line "^#line $(($SOURCE_O + 1)) \"parser.c\"" parser.c)
    [ "$SOURCE_S" -eq $((SOURCE_O - 1)) ]
    [ "$SOURCE_E" -eq $((SOURCE_O + 1)) ]

    COMMON_I=$(get_line ":COMMON:" input.peg)
    COMMON_O=$(get_line ":COMMON:" parser.c)
    COMMON_S=$(get_line "^#line $COMMON_I \"input.peg\"" parser.c)
    COMMON_E=$(get_line "^#line $(($COMMON_O + 1)) \"parser.c\"" parser.c)
    [ "$COMMON_S" -eq $((COMMON_O - 1)) ]
    [ "$COMMON_E" -eq $((COMMON_O + 1)) ]

    RULE1_I=$(get_line ":RULE1:" input.peg)
    RULE1_O=$(get_line ":RULE1:" parser.c)
    RULE1_S=$(get_line "^#line $RULE1_I \"input.peg\"" parser.c)
    RULE1_E=$(get_line "^#line $(($RULE1_O + 1)) \"parser.c\"" parser.c)
    [ "$RULE1_S" -eq $((RULE1_O - 1)) ]
    [ "$RULE1_E" -eq $((RULE1_O + 1)) ]

    RULE2_I=$(get_line ":RULE2:" input.peg)
    RULE2_O=$(get_line ":RULE2:" parser.c)
    RULE2_S=$(get_line "^#line $RULE2_I \"input.peg\"" parser.c)
    RULE2_E=$(get_line "^#line $(($RULE2_O + 1)) \"parser.c\"" parser.c)
    [ "$RULE2_S" -eq $((RULE2_O - 1)) ]
    [ "$RULE2_E" -eq $((RULE2_O + 1)) ]

    RULE3_I=$(get_line ":RULE3:" input.peg)
    RULE3_O=$(get_line ":RULE3:" parser.c)
    RULE3_S=$(get_line "^#line $RULE3_I \"input.peg\"" parser.c)
    RULE3_E=$(get_line "^#line $(($RULE3_O + 1)) \"parser.c\"" parser.c)
    [ "$RULE3_S" -eq $((RULE3_O - 1)) ]
    [ "$RULE3_E" -eq $((RULE3_O + 1)) ]

    CODE_I=$(get_line ":CODE:" input.peg)
    CODE_O=$(get_line ":CODE:" parser.c)
    CODE_S=$(get_line "^#line $CODE_I \"input.peg\"" parser.c)
    [ "$CODE_S" -eq $((CODE_O - 1)) ]
}
