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
#include "intern.h"
#include "numarray.h"
#include "routines.h"
#include "routines_p.h"
#include "vstring.h"
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

static void test_intern(void)
{
	const char *str = "asdfasfaskeopsdfksd";
	const char *symbol0 = intern(str);
	const char *symbol1, *symbol2, *symbol3;

	char *tmp = strdup (str);
	symbol1 = intern (tmp);
	free (tmp);
	symbol2 = intern (symbol0);
	symbol3 = intern (symbol1);

	TEST_CHECK (symbol0 == symbol1);
	TEST_CHECK (symbol1 == symbol2);
	TEST_CHECK (symbol2 == symbol3);
}

static void test_numarray(void)
{
	intArray *a = intArrayNew ();

	intArrayAdd(a, 0);
	intArrayAdd(a, 1);
	intArrayAdd(a, 2);

	TEST_CHECK (intArrayCount(a) == 3);
	TEST_CHECK (intArrayItem(a, 0) == 0);
	TEST_CHECK (intArrayItem(a, 1) == 1);
	TEST_CHECK (intArrayItem(a, 2) == 2);
	TEST_CHECK (intArrayLast(a) == 2);
	TEST_CHECK (intArrayRemoveLast(a) == 2);
	TEST_CHECK (intArrayLast(a) == 1);

	intArrayDelete(a);
}

static void test_routines_strrstr(void)
{
	TEST_CHECK(strcmp(strrstr("abcdcdb", "cd"), "cdb") == 0);
}

static void test_routines_filenameSansExtensionNew(void)
{
	char *bs;

	TEST_CHECK ((bs = filenameSansExtensionNew ("a.in", ".in"))
				&& strcmp(bs, "a") == 0);
	if (bs)
		eFree (bs);

	TEST_CHECK ((bs = filenameSansExtensionNew ("x/b.in", ".in"))
				&& strcmp(bs, "x/b") == 0);
	if (bs)
		eFree (bs);

	TEST_CHECK ((bs = filenameSansExtensionNew ("c.in.in", ".in.in"))
				&& strcmp(bs, "c") == 0);
	if (bs)
		eFree (bs);

	TEST_CHECK ((bs = filenameSansExtensionNew ("/y/d.in.in", ".in.in"))
				&& strcmp(bs, "/y/d") == 0);
	if (bs)
		eFree (bs);
}

static void test_vstring_ncats(void)
{
	vString *vstr = vStringNew ();

	vStringCatS (vstr, "abc");
	vStringNCatS (vstr, "def", 0);
	TEST_CHECK(strcmp (vStringValue (vstr), "abc") == 0);
	vStringClear (vstr);

	vStringCatS (vstr, "abc");
	vStringNCatS (vstr, "def", 1);
	TEST_CHECK(strcmp (vStringValue (vstr), "abcd") == 0);
	vStringClear (vstr);

	vStringCatS (vstr, "abc");
	vStringNCatS (vstr, "def", 2);
	TEST_CHECK(strcmp (vStringValue (vstr), "abcde") == 0);
	vStringClear (vstr);

	vStringCatS (vstr, "abc");
	vStringNCatS (vstr, "def", 3);
	TEST_CHECK(strcmp (vStringValue (vstr), "abcdef") == 0);
	vStringClear (vstr);

	vStringCatS (vstr, "abc");
	vStringNCatS (vstr, "def", 4);
	TEST_CHECK(strcmp (vStringValue (vstr), "abcdef") == 0);
	vStringClear (vstr);

	vStringDelete (vstr);
}

static void test_vstring_truncate_leading(void)
{
	vString *vstr = vStringNewInit ("   abcdefg");
	TEST_CHECK(vstr != NULL);

	vStringStripLeading (vstr);
	TEST_CHECK(strcmp(vStringValue(vstr), "abcdefg") == 0);

	vStringTruncateLeading (vstr, 3);
	TEST_CHECK(strcmp(vStringValue(vstr), "defg") == 0);

	vStringTruncateLeading (vstr, 0);
	TEST_CHECK(strcmp(vStringValue(vstr), "defg") == 0);

	vStringTruncateLeading (vstr, 100);
	TEST_CHECK(strcmp(vStringValue(vstr), "") == 0);

	vStringDelete (vstr);
}

static void test_vstring_eqc(void)
{
	vString *vstr = vStringNewInit ("abcdefg");
	TEST_CHECK(vstr != NULL);

	TEST_CHECK(vStringEqC(vstr, "abcdefg"));
	TEST_CHECK(!vStringEqC(vstr, "abcdefgz"));
	TEST_CHECK(!vStringEqC(vstr, "abcdef"));
	TEST_CHECK(!vStringEqC(vstr, ""));

	vStringDelete (vstr);
}

TEST_LIST = {
   { "fname/absolute",   test_fname_absolute   },
   { "fname/absolute+cache", test_fname_absolute_with_cache },
   { "fname/relative",   test_fname_relative   },
   { "htable/update",    test_htable_update    },
   { "htable/grow",      test_htable_grow      },
   { "intern",           test_intern           },
   { "numarray",         test_numarray         },
   { "routines/strrstr", test_routines_strrstr },
   { "routines/filenameSansExtensionNew", test_routines_filenameSansExtensionNew },
   { "vstring/ncats",    test_vstring_ncats    },
   { "vstring/truncate_leading", test_vstring_truncate_leading },
   { "vstring/EqC",      test_vstring_eqc },
   { NULL, NULL }     /* zeroed record marking the end of the list */
};
