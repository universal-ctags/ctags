/*
*   Copyright (c) 2022, Masatake YAMATO
*
*   This source code is released into the public domain.
*
*   Testing the fix for handling unescaping
*/

#ifndef TEST_FIELDS_H
#define TEST_FIELDS_H

#define NEXT()										\
	r = tagsNext (t, &e);							\
	if (r != TagSuccess)							\
	{												\
		fprintf (stderr, "error in tagsNext\n");	\
		return 1;									\
	}												\
	do {} while (0)

#define CHECK(EXP,FIELD)												\
	if (strcmp (EXP, e.FIELD) != 0)										\
	{																	\
		fprintf (stderr, "unexpected " #FIELD "(expected: %s, actual: %s) in tagsFirst\n", \
				 EXP, e.FIELD);											\
		return 1;														\
	} \
	do {} while (0)

#define CHECK_X(FIELD,EXP)												\
	{																	\
		unsigned short i;												\
		for (i = 0; i < e.fields.count; i++)							\
		{																\
			if (strcmp (e.fields.list [i].key, FIELD) == 0)				\
			{															\
				if (strcmp(e.fields.list [i].value, EXP) == 0)			\
					break;												\
				else													\
				{														\
					fprintf (stderr, "unexpected " #FIELD "(expected: %s, actual: %s) in tagsFirst\n", \
							 EXP, e.fields.list [i].value);				\
					return 1;											\
				}														\
			}															\
		}																\
		if (i >= e.fields.count)										\
		{																\
			fprintf (stderr, "unexpected " #FIELD " field is not found in tagsFirst (count: %u)\n", \
					 e.fields.count);									\
			return 1;													\
		}																\
	}

#define CHECK3(NAME,FILE,PAT)					\
	CHECK ((NAME), name);						\
	CHECK ((FILE), file);						\
	CHECK ((PAT),  address.pattern)

#define NEXT_CHECK3(NAME,FILE,PAT)				\
	NEXT ();									\
	CHECK3 (NAME,FILE,PAT)

#endif	/* !TEST_FIELDS_H */
