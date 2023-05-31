/*
*   Copyright (c) 2022 Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#include "general.h"

#include "acutest.h"
#include "fname.h"
#include "htable.h"
#include "routines.h"
#include <string.h>

static void test_fname_absolute(void)
{
	char *str;
	char *in;

#define T(INPUT,OUTPUT) \
	TEST_CHECK((in = eStrdup (INPUT), \
				strcmp((str = canonicalizeAbsoluteFileName (in)), OUTPUT) == 0)); \
	eFree (in); \
	eFree (str)

	T("/abc/efg/..", "/abc");
	T("/abc/efg/hij/..", "/abc/efg");
	T("/abc/efg/../", "/abc");
	T("/abc/efg/./", "/abc/efg");
	T("/abc/efg/./../.", "/abc");
	T("/abc/..", "/");

	T("..", "/");
	T(".", "/");
	T("a", "/a");
	T("abc", "/abc");
	T("", "/");

	T("../a", "/a");
	T("../abc", "/abc");
	T("./a", "/a");
	T("./abc", "/abc");
	T("a/../b", "/b");
	T("abc/../efg", "/efg");

	T("..//////a", "/a");
	T("..//..//..//a", "/a");
#undef T
}

static void test_fname_absolute_with_cache(void)
{
	struct canonFnameCacheTable *ct;
	bool absoluteOnly;
#define T(INPUT,OUTPUT) \
	if (!TEST_CHECK(strcmp(canonicalizeFileName (ct, INPUT), \
			       OUTPUT) == 0))				\
		fprintf(stderr, "	ACTUAL: %s (%s)\n", canonicalizeFileName (ct, INPUT), absoluteOnly? "absOnly": "relaOK")

	absoluteOnly = true;
	ct = canonFnameCacheTableNew ("/abc", absoluteOnly);
	T("/abc/input", "/abc/input");
	T("/abc/../input", "/input");
	T("/abc/../../input", "/input");
	T("/abc/.././../input", "/input");
	T("/abc/./input", "/abc/input");
	T("/abc/.//input", "/abc/input");
	T("/abc/.//.//input", "/abc/input");
	T("/input", "/input");
	T("/./input", "/input");
	T("/../z/../input", "/input");
	T("/../z/../..input", "/..input");

	T("input", "/abc/input");
	T("./input", "/abc/input");
	T("../input", "/input");
	T("..//input", "/input");
	T(".././input", "/input");
	T("..//.//input", "/input");
	T("../d/input", "/d/input");
	T("../d/../input", "/input");
	T("../d/..//input", "/input");
	T("../d/..///./input", "/input");
	canonFnameCacheTableDelete (ct);

	absoluteOnly = true;
	ct = canonFnameCacheTableNew ("/abc/efg", absoluteOnly);
	T("input", "/abc/efg/input");
	T("../input", "/abc/input");
	T("..//input", "/abc/input");
	T(".././input", "/abc/input");
	T("..//.//input", "/abc/input");
	T("../d/input", "/abc/d/input");
	T("../d/../input", "/abc/input");
	T("../d/..//input", "/abc/input");
	T("../d/..///./input", "/abc/input");
	T("../d/..///./input/.././input", "/abc/input");
	T("", "/abc/efg");
	T(".", "/abc/efg");
	T("./", "/abc/efg");
	T("./..", "/abc");
	T("./.", "/abc/efg");
	T("././//", "/abc/efg");
	T("././//.", "/abc/efg");
	T("././../efg/.", "/abc/efg");
	T("..", "/abc");
	T("../..", "/");
	T("../../", "/");
	T("../..", "/");
	T("../../..", "/");
	T("../../../..", "/");
	T("../././..", "/");
	T("./././..", "/abc");
	T("...", "/abc/efg/...");
	T(".../", "/abc/efg/...");
	T("...//", "/abc/efg/...");
	T("..././", "/abc/efg/...");
	T("...//./", "/abc/efg/...");
	T("...//.//", "/abc/efg/...");
	T("..././/", "/abc/efg/...");
	T("..././.../", "/abc/efg/.../...");
	canonFnameCacheTableDelete (ct);
#undef T
}

static void test_fname_relative(void)
{
	struct canonFnameCacheTable *ct;
	bool absoluteOnly;
#define T(INPUT,OUTPUT) \
	if (!TEST_CHECK(strcmp(canonicalizeFileName (ct, INPUT), \
			       OUTPUT) == 0))				\
		fprintf(stderr, "	ACTUAL: %s (%s)\n", canonicalizeFileName (ct, INPUT), absoluteOnly? "absOnly": "relaOK")

	absoluteOnly = false;
	ct = canonFnameCacheTableNew ("/abc", absoluteOnly);
	T("/abc/input", "/abc/input");
	T("/abc/../input", "/input");
	T("/abc/../../input", "/input");
	T("/abc/.././../input", "/input");
	T("/abc/./input", "/abc/input");
	T("/abc/.//input", "/abc/input");
	T("/abc/.//.//input", "/abc/input");
	T("/input", "/input");
	T("/./input", "/input");
	T("/../z/../input", "/input");
	T("/../z/../..input", "/..input");

	T("input", "input");
	T("./input", "input");
	T("../input", "/input");
	T("..//input", "/input");
	T(".././input", "/input");
	T("..//.//input", "/input");
	T("../d/input", "/d/input");
	T("../d/../input", "/input");
	T("../d/..//input", "/input");
	T("../d/..///./input", "/input");
	canonFnameCacheTableDelete (ct);

	absoluteOnly = false;
	ct = canonFnameCacheTableNew ("/abc/efg", absoluteOnly);
	T("input", "input");
	T("../input", "/abc/input");
	T("..//input", "/abc/input");
	T(".././input", "/abc/input");
	T("..//.//input", "/abc/input");
	T("../d/input", "/abc/d/input");
	T("../d/../input", "/abc/input");
	T("../d/..//input", "/abc/input");
	T("../d/..///./input", "/abc/input");
	T("../d/..///./input/.././input", "/abc/input");
	T("", ".");
	T(".", ".");
	T("./", ".");
	T("./..", "/abc");
	T("./.", ".");
	T("././//", ".");
	T("././//.", ".");
	T("././../efg/.", ".");
	T("..", "/abc");
	T("../..", "/");
	T("../../", "/");
	T("../..", "/");
	T("../../..", "/");
	T("../../../..", "/");
	T("../././..", "/");
	T("./././..", "/abc");
	T("...", "...");
	T(".../", "...");
	T("...//", "...");
	T("..././", "...");
	T("...//./", "...");
	T("...//.//", "...");
	T("..././/", "...");
	T("..././.../", ".../...");
	canonFnameCacheTableDelete (ct);
#undef T
}

static void test_htable_update(void)
{
	hashTable *htable = hashTableNew (3, hashCstrhash, hashCstreq,
									  eFree, NULL);
	TEST_CHECK(htable != NULL);

	hashTablePutItem (htable, strdup("a"), "A");
	TEST_CHECK (hashTableUpdateItem (htable,  "a", "B") == true);
	TEST_CHECK (hashTableUpdateItem (htable,  "b", "B") == false);
	TEST_CHECK (strcmp (hashTableGetItem (htable, "a"), "B") == 0);
	TEST_CHECK (hashTableUpdateOrPutItem (htable,  "a", "C") == true);
	TEST_CHECK (hashTableUpdateOrPutItem (htable,  strdup("x"), "X") == false);
	TEST_CHECK (strcmp (hashTableGetItem (htable, "x"), "X") == 0);
	hashTableDelete(htable);
}

static void test_htable_grow(void)
{
	hashTable *htable;
	int i;
	char keyBuf[20];

	htable = hashTableNew (3, hashCstrhash, hashCstreq, eFree, NULL);

	for (i = 0; i < 1000; ++i)
	{
		snprintf(keyBuf, sizeof(keyBuf), "str_%d", i);
		hashTablePutItem (htable, strdup(keyBuf), strdup(keyBuf));
	}

	TEST_CHECK (strcmp (hashTableGetItem (htable, "str_123"), "str_123") == 0);
	hashTableDelete(htable);
}

static void test_routines_strrstr(void)
{
	TEST_CHECK(strcmp(strrstr("abcdcdb", "cd"), "cdb") == 0);
}

TEST_LIST = {
   { "fname/absolute",   test_fname_absolute   },
   { "fname/absolute+cache", test_fname_absolute_with_cache },
   { "fname/relative",   test_fname_relative   },
   { "htable/update",    test_htable_update    },
   { "htable/grow",      test_htable_grow      },
   { "routines/strrstr", test_routines_strrstr },
   { NULL, NULL }     /* zeroed record marking the end of the list */
};
