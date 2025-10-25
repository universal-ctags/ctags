======================================================================
Changes in 6.2.1
======================================================================

This release fixes some critical bugs reported/found after releasing
6.2.0.

commit 5445a348f290450b3f230cce221a8d5fe9a35f4a
Author: Bernát Gábor <bgabor8@bloomberg.net>
Date:   Fri Oct 24 08:51:57 2025 -0700

    R6: fix valgrind error

    Signed-off-by: Bernát Gábor <bgabor8@bloomberg.net>
    (cherry picked from commit 2bfc47b01e1e51ad38bbd9485bf0b0486b2ae42b)

commit 466bf9b416415ef0068c29858e2bf1620acc91aa
Author: Bernát Gábor <bgabor8@bloomberg.net>
Date:   Fri Oct 24 07:46:41 2025 -0700

    tokeninfo: fix null pointer crash in tokenDelete function

    The tokenDelete() function in main/tokeninfo.c was being called with NULL
    pointers from the R6 parser error handling code, causing segmentation faults.

    The R6 parser in parsers/r-r6class.c had a comment stating "tokenDelete
    accepts NULL" but the function did not actually handle NULL pointers safely.
    This occurred when parsing malformed R6 syntax such as:
    - R6::SomethingElse() (wrong function after R6 namespace)
    - R6:: (incomplete namespace reference)
    - R6::R6Clas() (typo in R6Class)

    The fix ensures ctags handles malformed R6 syntax gracefully by falling back
    to parsing assignments as regular global variables instead of crashing.

      Fixes segmentation fault when processing certain R6Class syntax patterns.

    Signed-off-by: Bernát Gábor <bgabor8@bloomberg.net>
    (cherry picked from commit a60763cfdba6d0744836a1ebab48d19e0d7cf4fe)

commit bc67b9257c60b8abaf567a4ec661fc75bdf32cfa
Author: Colomban Wendling <ban@herbesfolles.org>
Date:   Fri Jul 25 00:36:26 2025 +0200

    ada: Fix crash with `is` at EOF

    Fixes #4284.

    (cherry picked from commit 33d1d1b493464dab35b71f09e9d308f97c65e58e)

commit 0869fb8ec78634f1cd199b5676231e94111e3ddf
Author: Masatake YAMATO <yamato@redhat.com>
Date:   Sat Sep 27 02:36:20 2025 +0900

    Cxx: limit recursion when parsing too deep blocks

    Fixed #4292.

    Signed-off-by: Masatake YAMATO <yamato@redhat.com>
    (cherry picked from commit 0be19d0fc565f1bda46e76c11b3b2fc8a4c7bbee)

commit 4a7d896424838bf8bb5e04037bd6918305b64e09
Author: Masatake YAMATO <yamato@redhat.com>
Date:   Tue Jun 3 02:16:23 2025 +0900

    fixup! C++: Update the parser version to 2.2

    In d469bf986acc7837514680434e90bc6087fe2827, I forgot to update
    versionCurrent and versionAge member of the parser.

    (This means the parser versioning of ctags-6.2 is broken.
    I must backport this change to ctags-6.2 and release 6.2.1.)

    Signed-off-by: Masatake YAMATO <yamato@redhat.com>
    (cherry picked from commit 556fc03c340eb0e1ef45e1e6aafbfdbb0bd2b086)

commit 3805034ceece73c295e87c0ef5f12e02995ab0e0
Author: Masatake YAMATO <yamato@redhat.com>
Date:   Sat Jul 26 01:25:21 2025 +0900

    circleci: use fedora:42 for cross-build tests targeting aarch64

    Signed-off-by: Masatake YAMATO <yamato@redhat.com>
    (cherry picked from commit 21769fe58d8aa6a05f7f6f2941e06ddff5a6da68)
