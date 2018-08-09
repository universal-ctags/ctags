# this is a test of comments set(DO_NOT_TAG "foo")

#[[
multi-linecomments
option(DO_NOT_TAG "foo" OFF)
] not the end
]]set(tag_this)

add_custom_target(# comment set(NO_TAG "foo")
    # anothe rline comment
    good_target# this is legal comment placement set(NO_TAG foo)
    ALL)

add_library(another_good_target# <-- target
    SHARED # set(NO_TAG bar)
    gmock-all.cc
    )
