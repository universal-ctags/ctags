/*
*   Copyright (c) 2017, Masatake YAMATO <yamato@redhat.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for VUE file.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include "parse.h"
#include "routines.h"
#include "read.h"
#include "mio.h"
#include "promise.h"

enum vueAreaType {
	vueAreaUnset,
	vueTemplateArea,
	vueScriptArea,
	vueStyleArea,
	COUNT_AREA,
};

const char *vueDefaultGuestParser[] = {
	[vueTemplateArea] = "HTML",
	[vueScriptArea] = "JavaScript",
	[vueStyleArea] = "CSS",
};

struct vueParserState {
	enum vueAreaType areaType;
	unsigned long inputLine;
	unsigned long inputOffset;
};

struct vueCallbackData {
	const enum vueAreaType matchedAreaType;
	struct vueParserState *state;
};

static bool enterArea (const char *line,
			      const regexMatch *matches,
			      unsigned int count CTAGS_ATTR_UNUSED,
			      void *data)
{
	struct vueCallbackData *cdata = data;
	struct vueParserState *s = cdata->state;

	if (s->areaType != vueAreaUnset)
		return true;

	s->areaType = cdata->matchedAreaType;
	s->inputLine = getInputLineNumber ();

	if (matches[0].length < strlen(line))
		s->inputOffset = matches[0].length + 1;
	else
	{
		s->inputLine++;
		s->inputOffset = 0;
	}

	return true;
}

static bool leaveArea (const char *line CTAGS_ATTR_UNUSED,
			      const regexMatch *matches CTAGS_ATTR_UNUSED,
			      unsigned int count CTAGS_ATTR_UNUSED,
			      void *data)
{
	struct vueCallbackData *cdata = data;
	struct vueParserState *s = cdata->state;

	unsigned long endLine;

	if (s->areaType != cdata->matchedAreaType)
		return true;

	if (s->inputOffset == 0)
		return true;

	endLine = getInputLineNumber ();
	if (endLine == 0)
		goto out;

	if (s->inputLine >= endLine)
		goto out;

	makePromise (vueDefaultGuestParser[s->areaType],
				 s->inputLine, s->inputOffset,
				 endLine, 0, s->inputLine);

 out:
	memset (s, 0, sizeof (*s));
	return true;
}


static void initializeVueParser (langType language)
{
	static struct vueParserState parserState;

	memset (&parserState, 0, sizeof (parserState));

	static struct vueCallbackData callbackData [COUNT_AREA] = {
		[vueTemplateArea] = {
			.matchedAreaType = vueTemplateArea,
			.state = &parserState,
		},
		[vueScriptArea] = {
			.matchedAreaType = vueScriptArea,
			.state = &parserState,
		},
		[vueStyleArea] = {
			.matchedAreaType = vueStyleArea,
			.state = &parserState,
		},
	};
	addLanguageCallbackRegex (language, "^<template>",  "{exclusive}",
							  enterArea, NULL, callbackData + vueTemplateArea);
	addLanguageCallbackRegex (language, "^</template>", "{exclusive}",
							  leaveArea, NULL, callbackData + vueTemplateArea);
	addLanguageCallbackRegex (language, "^<script>",  "{exclusive}",
							  enterArea, NULL, callbackData + vueScriptArea);
	addLanguageCallbackRegex (language, "^</script>", "{exclusive}",
							  leaveArea, NULL, callbackData + vueScriptArea);
	addLanguageCallbackRegex (language, "^<style>",  "{exclusive}",
							  enterArea, NULL, callbackData + vueStyleArea);
	addLanguageCallbackRegex (language, "^</style>", "{exclusive}",
							  leaveArea, NULL, callbackData + vueStyleArea);
}

static void runVueParser (void)
{
	findRegexTags ();
}

extern parserDefinition* VueParser (void)
{
	static const char *const extensions [] = { "vue", NULL };
	parserDefinition* const def = parserNew ("Vue");
	def->extensions = extensions;
	def->initialize = initializeVueParser;
	def->method     = METHOD_REGEX;
	def->parser     = runVueParser;
	return def;
}
