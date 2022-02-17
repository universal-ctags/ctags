/*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, Inc.
*   Copyright (c) 2022, Vasily Kulikov
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for applying yaml traversing.
*/

#include "general.h"  /* must always come first */

#include <inttypes.h>

#include "debug.h"
#include "entry.h"
#include "htable.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "subparser.h"
#include "trace.h"
#include "types.h"
#include "yaml.h"

#include "numarray.h"
#include "keyword.h"
#include "trashbox.h"


#define entry(X) [X] = #X
static const char *tokenTypeName [] = {
    entry (YAML_NO_TOKEN),
    entry (YAML_STREAM_START_TOKEN),
    entry (YAML_STREAM_END_TOKEN),
    entry (YAML_VERSION_DIRECTIVE_TOKEN),
    entry (YAML_TAG_DIRECTIVE_TOKEN),
    entry (YAML_DOCUMENT_START_TOKEN),
    entry (YAML_DOCUMENT_END_TOKEN),
    entry (YAML_BLOCK_SEQUENCE_START_TOKEN),
    entry (YAML_BLOCK_MAPPING_START_TOKEN),
    entry (YAML_BLOCK_END_TOKEN),
    entry (YAML_FLOW_SEQUENCE_START_TOKEN),
    entry (YAML_FLOW_SEQUENCE_END_TOKEN),
    entry (YAML_FLOW_MAPPING_START_TOKEN),
    entry (YAML_FLOW_MAPPING_END_TOKEN),
    entry (YAML_BLOCK_ENTRY_TOKEN),
    entry (YAML_FLOW_ENTRY_TOKEN),
    entry (YAML_KEY_TOKEN),
    entry (YAML_VALUE_TOKEN),
    entry (YAML_ALIAS_TOKEN),
    entry (YAML_ANCHOR_TOKEN),
    entry (YAML_TAG_TOKEN),
    entry (YAML_SCALAR_TOKEN),
};

static void yamlInit (yaml_parser_t *yaml)
{
	const unsigned char* data;
	size_t size;

	yaml_parser_initialize (yaml);

	data = getInputFileData (&size);
	Assert (data);

	yaml_parser_set_input_string (yaml, data, size);
}

static void yamlFini (yaml_parser_t *yaml)
{
	yaml_parser_delete (yaml);
}

extern void attachYamlPosition (tagEntryInfo *tag, yaml_token_t *token, bool asEndPosition)
{
	unsigned int ln = token->start_mark.line + 1;

	if (asEndPosition)
		tag->extensionFields.endLine = ln;
	else
	{
		tag->lineNumber = ln;
		tag->filePosition = getInputFilePositionForLine (tag->lineNumber);
	}
}

enum YamlKind {
	K_ANCHOR,
};

enum YamlAnchorRole {
	R_ANCHOR_ALIAS,
};

static roleDefinition YamlAnchorRoles [] = {
	{ true, "alias", "alias" },
};

static kindDefinition YamlKinds [] = {
	{ true,  'a', "anchor", "anchors",
	  .referenceOnly = false, ATTACH_ROLES(YamlAnchorRoles) },
};

static void handlYamlToken (yaml_token_t *token)
{
	tagEntryInfo tag;

	if (token->type == YAML_ANCHOR_TOKEN)
	{
		initTagEntry (&tag, (char *)token->data.anchor.value,
					  K_ANCHOR);
		attachYamlPosition (&tag, token, false);
		makeTagEntry (&tag);
	}
	else if (token->type == YAML_ALIAS_TOKEN)
	{
		initRefTagEntry (&tag, (char *)token->data.alias.value,
						 K_ANCHOR, R_ANCHOR_ALIAS);
		attachYamlPosition (&tag, token, false);
		makeTagEntry (&tag);
	}
}

static void findYamlTags (void)
{
	subparser *sub;
	yaml_parser_t yaml;
	yaml_token_t token;
	bool done;

	yamlInit (&yaml);

	findRegexTags ();

	sub = getSubparserRunningBaseparser();
	if (sub)
		chooseExclusiveSubparser (sub, NULL);

	done = false;
	while (!done)
	{
		if (!yaml_parser_scan (&yaml, &token))
			break;

		handlYamlToken (&token);
		foreachSubparser(sub, false)
		{
			enterSubparser (sub);
			((yamlSubparser *)sub)->newTokenNotfify ((yamlSubparser *)sub, &token);
			leaveSubparser ();
		}

		TRACE_PRINT("yaml token:%s<%d>@Line:%"PRIuPTR"", tokenTypeName[token.type], token.type,
				token.start_mark.line + 1);
		if (isTraced() && token.type == YAML_SCALAR_TOKEN)
		{
			TRACE_PRINT_FMT("	");
			for (size_t i = 0; i < token.data.scalar.length; i++)
				TRACE_PRINT_FMT("%c", token.data.scalar.value[i]);
			TRACE_PRINT_NEWLINE();
		}

		if (token.type == YAML_STREAM_END_TOKEN)
			done = true;

		yaml_token_delete (&token);
	}
	yamlFini (&yaml);
}

extern parserDefinition* YamlParser (void)
{
	static const char *const extensions [] = { "yml", "yaml", NULL };
	parserDefinition* const def = parserNew ("Yaml");

	def->kindTable = YamlKinds;
	def->extensions = extensions;
	def->parser = findYamlTags;
	def->useCork = CORK_QUEUE;
	def->kindTable = YamlKinds;
	def->kindCount = ARRAY_SIZE (YamlKinds);
	def->useMemoryStreamInput = true;

	return def;
}

/*
 * Experimental Ypath code
 */
struct ypathTypeStack {
	yaml_token_type_t type;
	int key;
	struct ypathTypeStack *next;
};

extern int ypathCompileTable (langType language, tagYpathTable *table, int keywordId)
{
	vString *tmpkey = vStringNew();
	intArray *code = intArrayNew ();

	for (const char *ypath = table->ypath; true; ypath++)
	{
		if (*ypath == '/' || *ypath == '\0')
		{
			if (!vStringIsEmpty (tmpkey))
			{
				int k;
				if (vStringLength (tmpkey) == 1 && (vStringValue (tmpkey)[0] == '*'))
					k = KEYWORD_NONE;
				else
				{
					k = lookupKeyword (vStringValue (tmpkey), language);
					if (k == KEYWORD_NONE)
					{
						char *keyword = DEFAULT_TRASH_BOX(vStringStrdup (tmpkey), eFree);
						k = keywordId++;
						addKeyword (keyword, language, k);
					}
				}
				intArrayAdd (code, k);
				vStringClear (tmpkey);
			}
			if (*ypath == '\0')
				break;
		}
		else
			vStringPut (tmpkey, *ypath);
	}

	intArrayReverse (code);
	table->code = code;

	vStringDelete (tmpkey);
	return keywordId;
}

extern void ypathCompileTables (langType language, tagYpathTable tables[], size_t count, int keywordId)
{
	for (size_t i = 0; i < count; i++)
		keywordId = ypathCompileTable (language, tables + i, keywordId);
}

extern void ypathCompiledCodeDelete (tagYpathTable tables[], size_t count)
{
	for (size_t i = 0; i < count; i++)
	{
		intArrayDelete (tables[i].code);
		tables[i].code = NULL;
	}
}

extern void ypathPushType (yamlSubparser *yaml, yaml_token_t *token)
{
	struct ypathTypeStack *s;

	s = xMalloc (1, struct ypathTypeStack);

	s->next = yaml->ypathTypeStack;
	yaml->ypathTypeStack = s;

	s->type = token->type;
	s->key = KEYWORD_NONE;
}

extern void ypathPopType (yamlSubparser *yaml)
{
	struct ypathTypeStack *s;

	s = yaml->ypathTypeStack;
	yaml->ypathTypeStack = s->next;

	s->next = NULL;

	eFree (s);
}

extern void ypathPopAllTypes (yamlSubparser *yaml)
{
	while (yaml->ypathTypeStack)
		ypathPopType (yaml);
}

extern void ypathFillKeywordOfTokenMaybe (yamlSubparser *yaml, yaml_token_t *token, langType lang)
{
	if (!yaml->ypathTypeStack)
		return;

	int k = lookupKeyword ((char *)token->data.scalar.value, lang);
	yaml->ypathTypeStack->key = k;
}

static bool ypathStateStackMatch (struct ypathTypeStack *stack,
								  intArray *code, size_t offset)
{
	if (intArrayCount (code) - offset == 0)
	{
		if (stack == NULL)
			return true;
		else
			return false;
	}

	if (stack == NULL)
		return false;

	if (stack->key == intArrayItem (code, offset))
		return ypathStateStackMatch (stack->next, code, offset + 1);
	else
		return false;
}

extern void ypathHandleToken (yamlSubparser *yaml, yaml_token_t *token, int state,
							  tagYpathTable tables[], size_t count)
{
	if (!yaml->ypathTypeStack)
		return;

	for (size_t i = 0; i < count; i++)
	{
		if (tables[i].expected_state != state)
			continue;

		if (ypathStateStackMatch(yaml->ypathTypeStack, tables[i].code, 0))
		{
			tagEntryInfo tag;
			initTagEntry (&tag, (char *)token->data.scalar.value, tables[i].kind);
			attachYamlPosition (&tag, token, false);

			makeTagEntry (&tag);
			break;
		}
	}
}

#ifdef DO_TRACING
extern void ypathPrintTypeStack0(struct ypathTypeStack *stack)
{
	if (!stack)
	{
		TRACE_PRINT_NEWLINE();
		return;
	}

	tracePrintFmt("[%d] - ", stack->key);
	ypathPrintTypeStack0 (stack->next);
}

extern void ypathPrintTypeStack(yamlSubparser *yaml)
{
	ypathPrintTypeStack0 (yaml->ypathTypeStack);
}
#else
extern void ypathPrintTypeStack(yamlSubparser *yaml) {}
#endif
