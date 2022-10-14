/*
*   Copyright (c) 2022 Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#include "general.h"

#include "acutest.h"
#include "routines.h"
#include <string.h>

void test_routines_strrstr(void)
{
	TEST_CHECK(strcmp(strrstr("abcdcdb", "cd"), "cdb") == 0);
}

TEST_LIST = {
   { "routines/strrstr", test_routines_strrstr },
   { NULL, NULL }     /* zeroed record marking the end of the list */
};
