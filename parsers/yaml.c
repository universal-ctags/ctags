/*
*   Copyright (c) 2016, Masatake YAMATO
*   Copyright (c) 2016, Red Hat, Inc.
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
#include "types.h"
#include "yaml.h"



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

		verbose("yaml token:%s<%d>@Line:%"PRIuPTR"\n", tokenTypeName[token.type], token.type,
				token.start_mark.line + 1);
		if (token.type == YAML_STREAM_END_TOKEN)
			done = true;

		yaml_token_delete (&token);
	}
	yamlFini (&yaml);
}

extern parserDefinition* YamlParser (void)
{
	static const char *const extensions [] = { "yml", NULL };
	parserDefinition* const def = parserNew ("Yaml");

	def->kindTable = YamlKinds;
	def->extensions = extensions;
	def->parser     = findYamlTags;
	def->useCork    = CORK_QUEUE;
	def->kindTable         = YamlKinds;
	def->kindCount     = ARRAY_SIZE (YamlKinds);
	def->useMemoryStreamInput = true;

	return def;
}
