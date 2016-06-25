/*
*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, K.K.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for java properties
*   https://docs.oracle.com/javase/7/docs/api/java/util/Properties.html
*/

#include "general.h"	/* must always come first */
#include "parse.h"
#include "read.h"
#include "vstring.h"

typedef enum {
	K_KEY
} javaPropertiesKind;

static kindOption JavaPropertiesKinds [] = {
	{ TRUE, 'k', "key", "keys" },
};

static const unsigned char *skipWhiteSpace (const unsigned char *line)
{
	while (*line == ' '
	       || *line == '\t'
	       || *line == '\f')
		line++;
	return line;
}

static boolean doesValueContinue (const unsigned char *line)
{
	boolean in_escaping = FALSE;
	while (*line != '\0')
	{
		if (in_escaping)
			in_escaping = FALSE;
		else if (*line == '\\')
			in_escaping = TRUE;
		line++;
	}
	return in_escaping;
}

static const unsigned char * extractKey (const unsigned char *line, vString *key)
{
	boolean in_escaping = FALSE;

	while (*line != '\0')
	{
		if (in_escaping)
		{
			vStringPut (key, *line);
			in_escaping = FALSE;
		}
		else if (*line == ':' || *line == '='
			 || *line == ' ' || *line == '\t' || *line == '\f')
		{
			line++;
			break;
		}
		else if (*line == '\\')
		{
			vStringPut (key, *line);
			in_escaping = TRUE;
		}
		else
			vStringPut (key, *line);
		line++;
	}
	return line;
}

static void findJavaPropertiesTags (void)
{
	const unsigned char *line;
	boolean in_value = FALSE;
	boolean value_continues;
	static vString *key;

	if (key == NULL)
		key = vStringNew ();
	else
		vStringClear (key);
	
	while ((line = readLineFromInputFile ()) != NULL)
	{
		if (in_value)
		{
			value_continues = doesValueContinue (line);
			if (!value_continues)
				in_value = FALSE;
			continue;
		}

		line = skipWhiteSpace (line);
		if (*line == '\0'
		    || *line == '!' || *line == '#')
			continue;

		line = extractKey (line, key);
		makeSimpleTag (key, JavaPropertiesKinds, K_KEY);
		vStringClear (key);

		value_continues = doesValueContinue (line);
		if (value_continues)
			in_value = TRUE;
	}
}

extern parserDefinition*
JavaPropertiesParser (void)
{
	static const char *const extensions [] = { "properties", NULL };
	parserDefinition* const def = parserNew ("JavaProperties");

	def->kinds = JavaPropertiesKinds;
	def->kindCount = ARRAY_SIZE (JavaPropertiesKinds);
	def->extensions = extensions;
	def->parser = findJavaPropertiesTags;
	return def;
}


