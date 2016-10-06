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

#include "debug.h"
#include "htable.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "types.h"
#include "meta-yaml.h"



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

extern void runYamlParser (const yamlCallback callback, void* userData)
{
	yaml_parser_t yaml;
	yaml_token_t token;
	bool done;

	yamlInit (&yaml);

	findRegexTags ();

	done = false;
	while (!done)
	{
		if (!yaml_parser_scan (&yaml, &token))
			break;

		callback (&token, userData);
		verbose("yaml token:%s<%d>@Line:%lu\n", tokenTypeName[token.type], token.type,
				token.start_mark.line + 1);
		if (token.type == YAML_STREAM_END_TOKEN)
			done = true;

		yaml_token_delete (&token);
	}
	yamlFini (&yaml);
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

static hashTable *yamlParserClients;
extern void registerYamlParserClient (struct yamlParserClient *client)
{
	if (!yamlParserClients)
		yamlParserClients = hashTableNew (5, hashInthash, hashInteq,
										  NULL, NULL);
	hashTablePutItem (yamlParserClients, &client->lang, client);
}

static void call_callback (void *key CTAGS_ATTR_UNUSED, void *value, void *user_data)
{
	yaml_token_t *token = user_data;
	struct yamlParserClient *client = value;

	if (!isLanguageEnabled (client->lang))
		return;

	if (token->type == YAML_STREAM_START_TOKEN
		&& client->inputStart)
		client->data = client->inputStart ();

	pushLanguage (client->lang);
	client->callback (token, client->data);
	popLanguage ();

	if (token->type == YAML_STREAM_END_TOKEN)
	{
		if (client->inputEnd)
			client->inputEnd (client->data);
		client->data = NULL;
	}
}

static void multiplexer (yaml_token_t *token, void *data)
{
	hashTableForeachItem (yamlParserClients, call_callback, token);
}

enum YamlKind {
	K_ANCHOR,
};

enum YamlAnchorRole {
	R_ANCHOR_ALIAS,
};

static roleDesc YamlAnchorRoles [] = {
	{ true, "alias", "alias" },
};

static kindOption YamlKinds [] = {
	{ true,  'a', "anchor", "anchros",
	  .referenceOnly = false, ATTACH_ROLES(YamlAnchorRoles) },
};

static void findYamlTags (void)
{
	runYamlParser (multiplexer, NULL);
}

static void yaml (yaml_token_t *token, void *data CTAGS_ATTR_UNUSED)
{
	tagEntryInfo tag;

	if (token->type == YAML_ANCHOR_TOKEN)
	{
		initTagEntry (&tag, (char *)token->data.anchor.value,
					  YamlKinds + K_ANCHOR);
		attachYamlPosition (&tag, token, false);
		makeTagEntry (&tag);
	}
	else if (token->type == YAML_ALIAS_TOKEN)
	{
		initRefTagEntry (&tag, (char *)token->data.alias.value,
						 YamlKinds + K_ANCHOR, R_ANCHOR_ALIAS);
		attachYamlPosition (&tag, token, false);
		makeTagEntry (&tag);
	}

}

static struct yamlParserClient YamlYamlClient = {
	.callback = yaml,
};

static void initializeYamlParser (const langType language)
{
	YamlYamlClient.lang = language;
	registerYamlParserClient (&YamlYamlClient);
}

extern parserDefinition* YamlParser (void)
{
	static const char *const extensions [] = { "yml", NULL };
	parserDefinition* const def = parserNew ("Yaml");

	def->kinds = YamlKinds;
	def->extensions = extensions;
	def->initialize = initializeYamlParser;
	def->parser     = findYamlTags;
	def->useCork    = true;
	def->kinds         = YamlKinds;
	def->kindCount     = ARRAY_SIZE (YamlKinds);

	return def;
}
